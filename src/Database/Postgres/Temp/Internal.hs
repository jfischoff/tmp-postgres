{-# LANGUAGE RecordWildCards, LambdaCase, ScopedTypeVariables #-}
module Database.Postgres.Temp.Internal where
import System.IO.Temp
import System.Process
import System.Process.Internals
import Control.Concurrent
import System.IO
import System.Timeout
import System.Posix.Signals
import System.Exit
import System.Directory
import Network.Socket
import Control.Exception
import Data.Typeable
import Data.List.Split
import Control.Monad.Trans.Either
import Text.Read (readMaybe)
import System.IO.Error
import Control.Monad.Trans.Class

openFreePort :: IO Int
openFreePort = bracket (socket AF_INET Stream defaultProtocol) close $ \s -> do
  localhost <- inet_addr "127.0.0.1"
  bind s (SockAddrInet aNY_PORT localhost)
  listen s 1
  fromIntegral <$> socketPort s

waitForDB :: FilePath -> Int -> IO ()
waitForDB mainDir port
  = handle (\(_ :: IOException) -> threadDelay 10000 >> waitForDB mainDir port)
  $ do bracket
        (socket AF_UNIX Stream 0)
        close
        $ \sock -> connect sock
                 $ SockAddrUnix
                 $ mainDir ++ "/.s.PGSQL." ++ show port

data DB = DB
  { mainDir          :: FilePath
  , connectionString :: String
  , pid              :: ProcessHandle
  }

start :: IO (Either StartError DB)
start = startWithHandles stdout stderr

fourth :: (a, b, c, d) -> d
fourth (_, _, _, x) = x

shellWith :: Handle -> Handle -> String -> CreateProcess
shellWith stdOut stdErr cmd =
  (shell cmd)
    { std_err = UseHandle stdErr
    , std_out = UseHandle stdOut
    }

config :: FilePath -> String
config mainDir = unlines
  [ "listen_addresses = ''"
  , "shared_buffers = 12MB"
  , "fsync = off"
  , "synchronous_commit = off"
  , "full_page_writes = off"
  , "log_min_duration_statement = 0"
  , "log_connections = on"
  , "log_disconnections = on"
  , "unix_socket_directories = '" ++ mainDir ++ "'"
  ]

data StartError
  = InitDBFailed   ExitCode
  | CreateDBFailed ExitCode
  deriving (Show, Eq, Typeable)

instance Exception StartError

throwIfError :: (ExitCode -> StartError) -> ExitCode -> IO ()
throwIfError f e = case e of
  ExitSuccess -> return ()
  _       -> throwIO $ f e


pidString :: ProcessHandle -> IO String
pidString phandle = withProcessHandle phandle (\case
        OpenHandle p   -> return $ show p
        ClosedHandle _ -> return ""
        )

runProcessWith :: Handle -> Handle -> String -> String -> IO ExitCode
runProcessWith stdOut stdErr name cmd
  =   createProcess_ name (shellWith stdOut stdErr cmd)
  >>= waitForProcess . fourth

startWithHandles :: Handle -> Handle -> IO (Either StartError DB)
startWithHandles stdOut stdErr = do
  mainDir <- createTempDirectory "/tmp" "tmp-postgres"
  startWithHandlesAndDir mainDir stdOut stdErr

startWithHandlesAndDir :: FilePath
                       -> Handle
                       -> Handle
                       -> IO (Either StartError DB)
startWithHandlesAndDir mainDir stdOut stdErr = try $ do
  let dataDir = mainDir ++ "/data"

  throwIfError InitDBFailed =<<
    runProcessWith stdOut stdErr "initDB"
      ("initDB --nosync -E UNICODE -A trust -D " ++ dataDir)

  -- write config
  writeFile (dataDir ++ "/postgresql.conf") $ config mainDir
  port <- openFreePort
  -- slight race here, the port might not be free anymore!
  let connectionString = "postgresql:///test?host=" ++ mainDir ++ "&port=" ++ show port
  bracketOnError ( fmap (DB mainDir connectionString . fourth)
                 $ createProcess_ "postgres"
                     ( shellWith stdOut stdErr
                     $ "postgres -D "
                     ++ dataDir
                     ++ " -p "
                     ++ show port
                     )
                 )
                 (stop 10000000)
                 $ \result -> do
    writeFile (mainDir ++ "/postgres.pid") =<< pidString (pid result)
    waitForDB mainDir port

    throwIfError CreateDBFailed =<<
      runProcessWith stdOut stdErr "createDB"
        ( "createDB --host=" ++ mainDir
        ++ " --port=" ++ show port
        ++ " test"
        )

    return result

startAndLogToTmp :: IO (Either StartError DB)
startAndLogToTmp = do
  mainDir <- createTempDirectory "/tmp" "tmp-postgres"

  stdOutFile <- openFile (mainDir ++ "/" ++ "output.txt") WriteMode
  stdErrFile <- openFile (mainDir ++ "/" ++ "output.txt") WriteMode

  startWithHandlesAndDir mainDir stdOutFile stdErrFile

data Result
  = Success
  | TimedOut Int
  | ErrorCode Int
  | Failed String
  deriving (Show)

exitCodeToResult :: ExitCode -> IO Result
exitCodeToResult = \case
  ExitSuccess   -> return Success
  ExitFailure x -> return $ ErrorCode x

kill :: Int -> ProcessHandle -> IO Result
kill waitTime phandle = withProcessHandle phandle $ \case
  OpenHandle pid -> TimedOut waitTime <$ signalProcess killProcess pid
  ClosedHandle exitCode -> exitCodeToResult exitCode

stop :: Int -> DB -> IO Result
stop waitTime DB {..} = do

  stopResult <- timeout waitTime
              $ terminateProcess pid >> waitForProcess pid

  result <- case stopResult of
    Nothing -> kill waitTime pid
    Just exitCode -> exitCodeToResult exitCode

  removeDirectoryRecursive mainDir

  return result

at :: Int -> e -> [a] -> Either e a
at index err xs = case drop index xs of
  x : _ -> Right x
  _     -> Left err

-- create a DB record from connectionString
parseMainDir :: String -> Either String String
parseMainDir connectionString = do
  tailPeice <- at 1 "No question mark in connection string" $ splitOn "?" connectionString
  hostPart  <- at 0 "No host portion of connection string"  $ splitOn "&" tailPeice
  at 1 "No host value of connection string" $ splitOn "=" hostPart

catchMissingFile :: FilePath -> IO a -> EitherT String IO a
catchMissingFile filepath action = EitherT $ catch (Right <$> action) $ \e ->
  if isDoesNotExistError e then
    return $ Left $ "Missing file:" ++ filepath
  else
    throwIO e

readPidFile :: String -> EitherT String IO ProcessHandle
readPidFile filePath = do
  let pidPath = filePath ++ "/postgres.pid"
  contents <- catchMissingFile pidPath $ readFile pidPath
  case readMaybe contents of
    Nothing -> left "Was unable to read postgres.pid's pid"
    Just pid -> lift $ mkProcessHandle pid False

parseDB :: String -> EitherT String IO DB
parseDB connectionString = do
  mainDir <- EitherT $ return $ parseMainDir connectionString
  pid     <- readPidFile mainDir
  return $ DB {..}

stopWithConnectionString :: String -> IO Result
stopWithConnectionString str =
  runEitherT (parseDB str) >>= \case
    Left msg -> return $ Failed msg
    Right x  -> stop 10000000 x