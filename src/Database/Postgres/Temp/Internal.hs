{-# LANGUAGE RecordWildCards, LambdaCase, ScopedTypeVariables #-}
module Database.Postgres.Temp.Internal where
import System.IO.Temp
import System.Process
import System.Process.Internals
import Control.Concurrent
import System.IO
import System.Exit
import System.Directory
import Network.Socket
import Control.Exception
import Data.Typeable

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

-- | start postgres and use the current processes stdout and stderr
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

-- | Start postgres and pass in handles for stdout and stderr
startWithHandles :: Handle
                 -- ^ stdout
                 -> Handle
                 -- ^ stdin
                 -> IO (Either StartError DB)
startWithHandles stdOut stdErr = do
  mainDir <- createTempDirectory "/tmp" "tmp-postgres"
  startWithHandlesAndDir mainDir stdOut stdErr


startWithHandlesAndDir :: FilePath
                       -> Handle
                       -> Handle
                       -> IO (Either StartError DB)
startWithHandlesAndDir = startWithLogger $ \_ -> return ()

data Event
  = InitDB
  | WriteConfig
  | FreePort
  | StartPostgres
  | WaitForDB
  | CreateDB
  | Finished
  deriving (Show, Eq, Enum, Bounded, Ord)

rmDirIgnoreErrors :: FilePath -> IO ()
rmDirIgnoreErrors mainDir =
  removeDirectoryRecursive mainDir `catch` (\(_ :: IOException) -> return ())

startWithLogger :: (Event -> IO ())
                -> FilePath
                -> Handle
                -> Handle
                -> IO (Either StartError DB)
startWithLogger logger mainDir stdOut stdErr = try $ flip onException (rmDirIgnoreErrors mainDir) $ do
  let dataDir = mainDir ++ "/data"

  logger InitDB
  initDBExitCode <- runProcessWith stdOut stdErr "initDB"
      ("initDB --nosync -E UNICODE -A trust -D " ++ dataDir)
  throwIfError InitDBFailed initDBExitCode

  logger WriteConfig
  writeFile (dataDir ++ "/postgresql.conf") $ config mainDir

  logger FreePort
  port <- openFreePort
  -- slight race here, the port might not be free anymore!
  let connectionString = "postgresql:///test?host=" ++ mainDir ++ "&port=" ++ show port
  logger StartPostgres
  bracketOnError ( fmap (DB mainDir connectionString . fourth)
                 $ createProcess_ "postgres"
                     ( shellWith stdOut stdErr
                     $ "postgres -D "
                     ++ dataDir
                     ++ " -p "
                     ++ show port
                     )
                 )
                 stop
                 $ \result -> do
    logger WaitForDB
    waitForDB mainDir port

    logger CreateDB
    throwIfError CreateDBFailed =<<
      runProcessWith stdOut stdErr "createDB"
        ( "createDB --host=" ++ mainDir
        ++ " --port=" ++ show port
        ++ " test"
        )

    logger Finished
    return result

-- | Start postgres and log it's all stdout to TEMP_DIR/output.txt and TEMP_DIR/error.txt
startAndLogToTmp :: IO (Either StartError DB)
startAndLogToTmp = do
  mainDir <- createTempDirectory "/tmp" "tmp-postgres"

  stdOutFile <- openFile (mainDir ++ "/" ++ "output.txt") WriteMode
  stdErrFile <- openFile (mainDir ++ "/" ++ "error.txt") WriteMode

  startWithHandlesAndDir mainDir stdOutFile stdErrFile

-- | Stop postgres and clean up the temporary database folder.
stop :: DB -> IO ExitCode
stop DB {..} = do
  terminateProcess pid
  result <- waitForProcess pid
  removeDirectoryRecursive mainDir
  return result