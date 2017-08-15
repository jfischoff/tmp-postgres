{-# LANGUAGE RecordWildCards, LambdaCase, ScopedTypeVariables, DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
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
import GHC.Generics
import System.Posix.Signals

openFreePort :: IO Int
openFreePort = bracket (socket AF_INET Stream defaultProtocol) close $ \s -> do
  localhost <- inet_addr "127.0.0.1"
  bind s (SockAddrInet aNY_PORT localhost)
  listen s 1
  fmap fromIntegral $ socketPort s

waitForDB :: Maybe FilePath -> Int -> IO ()
waitForDB mMainDir port = do
  let sockAddress = case mMainDir of
        Just mainDir -> SockAddrUnix $ mainDir ++ "/.s.PGSQL." ++ show port
        Nothing -> SockAddrInet (fromIntegral port) $ tupleToHostAddress (127, 0, 0, 1)
      sockFamily = maybe AF_INET (const AF_UNIX) mMainDir
  eresult <- try $ bracket (socket sockFamily Stream 0) close $ \sock -> connect sock sockAddress
  case eresult of
    Left (e :: IOError) -> threadDelay 10000 >> waitForDB mMainDir port
    Right _ -> return ()


data DB = DB
  { mainDir          :: FilePath
  -- ^ Temporary directory where the unix socket, logs and data directory live.
  , connectionString :: String
  -- ^ PostgreSQL connection string.
  , pid              :: ProcessHandle
  -- ^ The process handle for the @postgres@ process.
  }

data SocketClass = Localhost | Unix
  deriving (Show, Eq, Read, Ord, Enum, Bounded, Generic, Typeable)

-- | start postgres and use the current processes stdout and stderr
start :: [(String, String)]
      -- ^ Extra options which override the defaults
      -> IO (Either StartError DB)
start options = startWithHandles Unix options stdout stderr

-- | start postgres and use the current processes stdout and stderr
-- but use TCP on localhost instead of a unix socket.
startLocalhost ::  [(String, String)]
               -- ^ Extra options which override the defaults
               -> IO (Either StartError DB)
startLocalhost options = startWithHandles Localhost options stdout stderr

fourth :: (a, b, c, d) -> d
fourth (_, _, _, x) = x

procWith :: Handle -> Handle -> String -> [String] -> CreateProcess
procWith stdOut stdErr cmd args =
  (proc cmd args)
    { std_err = UseHandle stdErr
    , std_out = UseHandle stdOut
    }

config :: Maybe FilePath -> String
config mMainDir = unlines $
  [ "shared_buffers = 12MB"
  , "fsync = off"
  , "synchronous_commit = off"
  , "full_page_writes = off"
  , "log_min_duration_statement = 0"
  , "log_connections = on"
  , "log_disconnections = on"
  , "client_min_messages = ERROR"
  ] ++ maybe ["listen_addresses = '127.0.0.1'"] (\x -> ["unix_socket_directories = '" ++ x ++ "'", "listen_addresses = ''"]) mMainDir

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

runProcessWith :: Handle -> Handle -> String -> String -> [String] -> IO ExitCode
runProcessWith stdOut stdErr name cmd args
  =   createProcess_ name (procWith stdOut stdErr cmd args)
  >>= waitForProcess . fourth

-- | Start postgres and pass in handles for stdout and stderr
startWithHandles :: SocketClass
                 -> [(String, String)]
                 -- ^ Extra options which override the defaults
                 -> Handle
                 -- ^ @stdout@
                 -> Handle
                 -- ^ @stderr@
                 -> IO (Either StartError DB)
startWithHandles socketClass options stdOut stdErr = do
  mainDir <- createTempDirectory "/tmp" "tmp-postgres"
  startWithHandlesAndDir socketClass options mainDir stdOut stdErr

startWithHandlesAndDir :: SocketClass
                       -> [(String, String)]
                       -> FilePath
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
                -> SocketClass
                -> [(String, String)]
                -> FilePath
                -> Handle
                -> Handle
                -> IO (Either StartError DB)
startWithLogger logger socketType options mainDir stdOut stdErr = try $ flip onException (rmDirIgnoreErrors mainDir) $ do
  let dataDir = mainDir ++ "/data"

  logger InitDB
  initDBExitCode <- runProcessWith stdOut stdErr "initdb"
      "initdb" ["-E", "UNICODE", "-A", "trust", "-D", dataDir]
  throwIfError InitDBFailed initDBExitCode

  logger WriteConfig
  writeFile (dataDir ++ "/postgresql.conf") $ config $ if socketType == Unix then Just mainDir else Nothing

  logger FreePort
  port <- openFreePort
  -- slight race here, the port might not be free anymore!
  let host = case socketType of
        Localhost -> "127.0.0.1"
        Unix -> mainDir
  let connectionString = "postgresql:///test?host=" ++ host ++ "&port=" ++ show port
  logger StartPostgres
  let extraOptions = map (\(key, value) -> "--" ++ key ++ "=" ++ value) options
  bracketOnError ( fmap (DB mainDir connectionString . fourth)
                 $ createProcess_ "postgres"
                     ( procWith stdOut stdErr
                      "postgres"
                      $ ["-D", dataDir, "-p", show port] ++ extraOptions
                     )
                 )
                 stop
                 $ \result -> do
    logger WaitForDB
    waitForDB (if socketType == Unix then Just mainDir else Nothing) port

    logger CreateDB
    let createDBHostArgs = case socketType of
          Unix -> ["-h", mainDir]
          Localhost -> ["-h", "127.0.0.1"]

    throwIfError CreateDBFailed =<<
      runProcessWith stdOut stdErr "createDB"
        "createdb" (createDBHostArgs ++ ["-p", show port, "test"])

    logger Finished
    return result

-- | Start postgres and log it's all stdout to {'mainDir'}\/output.txt and {'mainDir'}\/error.txt
startAndLogToTmp :: [(String, String)]
                 -- ^ Extra options which override the defaults
                 -> IO (Either StartError DB)
startAndLogToTmp options = do
  mainDir <- createTempDirectory "/tmp" "tmp-postgres"

  stdOutFile <- openFile (mainDir ++ "/" ++ "output.txt") WriteMode
  stdErrFile <- openFile (mainDir ++ "/" ++ "error.txt") WriteMode

  startWithHandlesAndDir Unix options mainDir stdOutFile stdErrFile

-- | Stop postgres and clean up the temporary database folder.
stop :: DB -> IO ExitCode
stop DB {..} = do
  withProcessHandle pid (\case
         OpenHandle p   -> signalProcess sigINT p
         ClosedHandle _ -> return ()
         )

  result <- waitForProcess pid
  removeDirectoryRecursive mainDir
  return result
