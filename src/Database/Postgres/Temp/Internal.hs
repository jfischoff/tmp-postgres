{-# LANGUAGE RecordWildCards, LambdaCase, ScopedTypeVariables, DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
{-# LANGUAGE TupleSections, DerivingStrategies, DerivingVia #-}

module Database.Postgres.Temp.Internal where

import Control.Concurrent (MVar, newMVar, threadDelay, withMVar)
import Control.Concurrent.Async (race_)
import Control.Exception
import Control.Monad (forever, void, (<=<))
import Data.Maybe
import qualified Data.ByteString.Char8 as BSC
import Data.Foldable (for_)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Typeable (Typeable)
import qualified Database.PostgreSQL.Simple as PG
import qualified Database.PostgreSQL.Simple.Options as PostgresClient
import GHC.Generics (Generic)
import Network.Socket.Free (getFreePort)
import System.Directory (removeDirectoryRecursive)
import System.Exit (ExitCode(..))
import System.IO
import System.IO.Temp (createTempDirectory)
import System.Posix.Signals (sigHUP, sigINT, signalProcess)
import System.Process (getPid, getProcessExitCode, proc, waitForProcess)
import System.Process.Internals
import Data.Traversable (for)
import Data.Monoid
import Data.Monoid.Generic
import Control.Applicative
import qualified Database.PostgreSQL.Simple.PartialOptions as Client
import System.Environment

throwMaybe :: Exception e => e -> Maybe a -> IO a
throwMaybe e = \case
  Nothing -> throwIO e
  Just  x -> pure x

waitForDB :: PostgresClient.Options -> IO ()
waitForDB options = do
  let theConnectionString = PostgresClient.toConnectionString options
  eresult <- try $ bracket (PG.connectPostgreSQL theConnectionString) PG.close $ \_ -> return ()
  case eresult of
    Left (_ :: IOError) -> threadDelay 10000 >> waitForDB options
    Right _ -> return ()

-- A helper for dealing with locks
withLock :: MVar a -> IO b -> IO b
withLock m f = withMVar m (const f)

-------------------------------------------------------------------------------
-- A useful type of options
-------------------------------------------------------------------------------
data Lastoid a = Replace a | Mappend a

instance Semigroup a => Semigroup (Lastoid a) where
  x <> y = case (x, y) of
    (r@Replace {}, _        ) -> r
    (Mappend a   , Replace b) -> Replace $ a <> b
    (Mappend a   , Mappend b) -> Mappend $ a <> b

instance Monoid a => Monoid (Lastoid a) where
  mempty = Mappend mempty
  mappend = (<>)

-------------------------------------------------------------------------------
-- ProcessOptions
-------------------------------------------------------------------------------
data PartialProcessOptions = PartialProcessOptions
  { partialProcessOptionsEnvVars :: Lastoid [(String, String)]
  , partialProcessOptionsCmdLine :: Lastoid [String]
  , partialProcessOptionsStdIn   :: Last Handle
  , partialProcessOptionsStdOut  :: Last Handle
  , partialProcessOptionsStdErr  :: Last Handle
  , partialProcessOptionsName    :: Last String
  }
  deriving stock (Generic)
  deriving Semigroup via GenericSemigroup PartialProcessOptions
  deriving Monoid    via GenericMonoid PartialProcessOptions

standardProcessOptions :: IO PartialProcessOptions
standardProcessOptions = do
  env <- getEnvironment
  pure mempty
    { partialProcessOptionsEnvVars = Replace env
    , partialProcessOptionsStdIn   = pure stdin
    , partialProcessOptionsStdOut  = pure stdout
    , partialProcessOptionsStdErr  = pure stderr
    }

data ProcessOptions = ProcessOptions
  { processOptionsEnvVars :: [(String, String)]
  , processOptionsCmdLine :: [String]
  , processOptionsStdIn   :: Handle
  , processOptionsStdOut  :: Handle
  , processOptionsStdErr  :: Handle
  , processOptionsName    :: String
  }

completeProcessOptions :: PartialProcessOptions -> Maybe ProcessOptions
completeProcessOptions PartialProcessOptions {..} = do
  processOptionsName <- getLast partialProcessOptionsName
  processOptionsEnvVars <- case partialProcessOptionsEnvVars of
    Replace x -> Just x
    Mappend _ -> Nothing
  processOptionsCmdLine <- case partialProcessOptionsCmdLine of
    Replace x -> pure x
    Mappend _ -> Nothing
  processOptionsStdIn  <- getLast partialProcessOptionsStdIn
  processOptionsStdOut <- getLast partialProcessOptionsStdOut
  processOptionsStdErr <- getLast partialProcessOptionsStdErr

  pure ProcessOptions {..}

data ProcessInput = ProcessInput
  { processInputEnvVars :: [(String, String)]
  , processInputCmdLine :: [String]
  , processInputName    :: String
  } deriving (Show, Eq, Ord)

-- envs might not be true
toProcessInput :: ProcessOptions -> ProcessInput
toProcessInput ProcessOptions {..} = ProcessInput
  { processInputEnvVars = processOptionsEnvVars
  , processInputCmdLine = processOptionsCmdLine
  , processInputName    = processOptionsName
  }

fourth :: (a, b, c, d) -> d
fourth (_, _, _, x) = x

evaluateProcess :: ProcessOptions -> IO ProcessHandle
evaluateProcess ProcessOptions {..} = fmap fourth $
  createProcess_ processOptionsName $
    (proc processOptionsName processOptionsCmdLine)
      { std_err = UseHandle processOptionsStdErr
      , std_out = UseHandle processOptionsStdOut
      , std_in  = UseHandle processOptionsStdIn
      , env     = Just processOptionsEnvVars
      }

executeProcess :: ProcessOptions -> IO ExitCode
executeProcess = waitForProcess <=< evaluateProcess

-------------------------------------------------------------------------------
-- Events and Exceptions
-------------------------------------------------------------------------------

data StartError
  = InitDBFailed   ExitCode
  | CreateDBFailed [String] ExitCode
  | StartPostgresFailed PostgresInput ExitCode
  | StartPostgresDisappeared PostgresInput
  | InitDbCompleteOptions
  | CreateDbCompleteOptions
  | PostgresCompleteOptions
  | ClientCompleteOptions
  deriving (Show, Eq, Typeable)

instance Exception StartError

data Event
  = InitDB
  | WriteConfig
  | FreePort
  | StartPostgres
  | WaitForDB
  | CreateDB
  | Finished
  deriving (Show, Eq, Enum, Bounded, Ord)

-------------------------------------------------------------------------------
-- PartialSocketClass
-------------------------------------------------------------------------------

data PartialSocketClass = PIpSocket (Maybe String) | PUnixSocket (Maybe FilePath)
  deriving stock (Show, Eq, Read, Ord, Generic, Typeable)

instance Semigroup PartialSocketClass where
  x <> y = case (x, y) of
    (PIpSocket   a, PIpSocket b) -> PIpSocket $ a <|> b
    (a@(PIpSocket _), PUnixSocket _) -> a
    (PUnixSocket _, a@(PIpSocket _)) -> a
    (PUnixSocket a, PUnixSocket b) -> PUnixSocket $ a <|> b

instance Monoid PartialSocketClass where
  mempty = PUnixSocket Nothing

data SocketClass = IpSocket String | UnixSocket FilePath
  deriving (Show, Eq, Read, Ord, Generic, Typeable)

listenAddressConfig :: SocketClass -> [String]
listenAddressConfig = \case
  IpSocket ip    -> ["listen_addresses = '" <> ip <> "'"]
  UnixSocket dir ->
    [ "listen_addresses = ''"
    , "unix_socket_directories = '" <> dir <> "'"
    ]

socketClassToHost :: SocketClass -> String
socketClassToHost = \case
  IpSocket ip    -> ip
  UnixSocket dir -> dir

startPartialSocketClass :: PartialSocketClass -> (SocketClass -> IO a) -> IO a
startPartialSocketClass theClass f = case theClass of
  PIpSocket mIp -> f $ IpSocket $ fromMaybe "127.0.0.1" mIp
  PUnixSocket mFilePath -> do
    let (dirCreate, dirDelete) = case mFilePath of
          Nothing -> (createTempDirectory "/tmp" "tmp-postgres-socket", rmDirIgnoreErrors)
          Just x  -> (pure x, const $ pure ())
    bracketOnError dirCreate dirDelete $ \socketPath -> f $ UnixSocket socketPath

-------------------------------------------------------------------------------
-- CommonOptions
-------------------------------------------------------------------------------

rmDirIgnoreErrors :: FilePath -> IO ()
rmDirIgnoreErrors mainDir =
  removeDirectoryRecursive mainDir `catch` (\(_ :: IOException) -> return ())

data CommonOptions = CommonOptions
  { commonOptionsDbName      :: String
  , commonOptionsDataDir     :: FilePath
  , commonOptionsPort        :: Int
  , commonOptionsSocketClass :: SocketClass
  , commonOptionsClientOptions :: Client.PartialOptions
  , commonOptionsLogger      :: Event -> IO ()
  }

data PartialCommonOptions = PartialCommonOptions
  { partialCommonOptionsDbName        :: Maybe String
  , partialCommonOptionsDataDir       :: Maybe FilePath
  , partialCommonOptionsPort          :: Maybe Int
  , partialCommonOptionsSocketClass   :: PartialSocketClass
  , partialCommonOptionsLogger        :: Maybe (Event -> IO ())
  , partialCommonOptionsClientOptions :: Client.PartialOptions
  }
  deriving stock (Generic)

instance Semigroup PartialCommonOptions where
  x <> y = PartialCommonOptions
    { partialCommonOptionsDbName      =
        partialCommonOptionsDbName x <|> partialCommonOptionsDbName y
    , partialCommonOptionsDataDir     =
        partialCommonOptionsDataDir x <|> partialCommonOptionsDataDir y
    , partialCommonOptionsPort        =
        partialCommonOptionsPort x <|> partialCommonOptionsPort y
    , partialCommonOptionsSocketClass =
        partialCommonOptionsSocketClass x <> partialCommonOptionsSocketClass y
    , partialCommonOptionsLogger      =
        partialCommonOptionsLogger x <|> partialCommonOptionsLogger y
    , partialCommonOptionsClientOptions =
      partialCommonOptionsClientOptions x <> partialCommonOptionsClientOptions y
    }

instance Monoid PartialCommonOptions where
  mempty = PartialCommonOptions
    { partialCommonOptionsDbName        = Nothing
    , partialCommonOptionsDataDir       = Nothing
    , partialCommonOptionsPort          = Nothing
    , partialCommonOptionsSocketClass   = mempty
    , partialCommonOptionsLogger        = Nothing
    , partialCommonOptionsClientOptions = mempty
    }

data CommonInput = CommonInput
  { commonInputDbName      :: String
  , commonInputDataDir     :: FilePath
  , commonInputSocketClass :: SocketClass
  , commonInputPort        :: Int
  }

commonOptionsToCommonInput :: CommonOptions -> CommonInput
commonOptionsToCommonInput CommonOptions {..} = CommonInput
  { commonInputDbName      = commonOptionsDbName
  , commonInputDataDir     = commonOptionsDataDir
  , commonInputSocketClass = commonOptionsSocketClass
  , commonInputPort        = commonOptionsPort
  }

commonOptionsToConnectionOptions :: CommonOptions -> Maybe PostgresClient.Options
commonOptionsToConnectionOptions CommonOptions {..}
  = either (const Nothing) pure
  $ Client.completeOptions
  $  commonOptionsClientOptions
  <> ( mempty
        { Client.host   = pure $ socketClassToHost commonOptionsSocketClass
        , Client.port   = pure commonOptionsPort
        , Client.dbname = pure commonOptionsDbName
        }
      )

startPartialCommonOptions :: PartialCommonOptions -> (CommonOptions -> IO a) -> IO a
startPartialCommonOptions PartialCommonOptions {..} f = do
  commonOptionsPort <- maybe getFreePort pure partialCommonOptionsPort

  let commonOptionsDbName        = fromMaybe "test" partialCommonOptionsDbName
      commonOptionsLogger        = fromMaybe (const $ pure ()) partialCommonOptionsLogger
      commonOptionsClientOptions = partialCommonOptionsClientOptions
      (dirCreate, dirDelete) = case partialCommonOptionsDataDir of
        Nothing -> (createTempDirectory "/tmp" "tmp-postgres-data", rmDirIgnoreErrors)
        Just x  -> (pure x, const $ pure ())

  bracketOnError dirCreate dirDelete $ \commonOptionsDataDir ->
    startPartialSocketClass partialCommonOptionsSocketClass $ \commonOptionsSocketClass ->
      f CommonOptions {..}

-- TODO stopPartialCommonOptions

-------------------------------------------------------------------------------
-- PostgresPlan
-------------------------------------------------------------------------------
data PartialPostgresPlan = PartialPostgresPlan
  { partialPostgresPlanConfig  :: Lastoid String
  , partialPostgresPlanOptions :: PartialProcessOptions
  } deriving stock (Generic)
    deriving Semigroup via GenericSemigroup PartialPostgresPlan
    deriving Monoid    via GenericMonoid PartialPostgresPlan

defaultConfig :: [String]
defaultConfig =
  [ "shared_buffers = 12MB"
  , "fsync = off"
  , "synchronous_commit = off"
  , "full_page_writes = off"
  , "log_min_duration_statement = 0"
  , "log_connections = on"
  , "log_disconnections = on"
  , "client_min_messages = ERROR"
  ]

defaultPostgresPlan :: CommonOptions -> PartialPostgresPlan
defaultPostgresPlan CommonOptions {..} = PartialPostgresPlan
  { partialPostgresPlanConfig  = Replace $ unlines $
      defaultConfig <> listenAddressConfig commonOptionsSocketClass
  , partialPostgresPlanOptions = mempty
      { partialProcessOptionsName = pure "postgres"
      }
  }

data PostgresPlan = PostgresPlan
  { postgresPlanConfig  :: String
  , postgresPlanOptions :: ProcessOptions
  }

completePostgresPlan :: PartialPostgresPlan -> Maybe PostgresPlan
completePostgresPlan PartialPostgresPlan {..} = do
  postgresPlanConfig <- case partialPostgresPlanConfig of
    Mappend _ -> Nothing
    Replace x -> Just x

  postgresPlanOptions <- completeProcessOptions partialPostgresPlanOptions

  pure PostgresPlan {..}

data PostgresInput = PostgresInput
  { postgresOptionsProcessOptions :: ProcessInput
  , postgresOptionsConfig  :: String
  } deriving (Show, Eq, Ord)

toPostgresInput :: PostgresPlan -> PostgresInput
toPostgresInput PostgresPlan {..} = PostgresInput
  { postgresOptionsProcessOptions = toProcessInput postgresPlanOptions
  , postgresOptionsConfig  = postgresPlanConfig
  }

data PostgresProcess = PostgresProcess
  { pidLock :: MVar ()
  -- ^ A lock used internally to makes sure access to 'pid' is serialized
  , pid :: IORef (Maybe ProcessHandle)
  -- ^ The process handle for the @postgres@ process.
  , options         :: PostgresClient.Options
  , postgresInput   :: PostgresInput
  }

-- | Force all connections to the database to close. Can be useful in some testing situations.
--   Called during shutdown as well.
terminateConnections :: PostgresProcess -> IO ()
terminateConnections PostgresProcess {..} = do
  let theConnectionString =
        BSC.unpack . PostgresClient.toConnectionString $ options
  e <- try $ bracket (PG.connectPostgreSQL $ BSC.pack theConnectionString)
          PG.close
          $ \conn -> do
            let q = "select pg_terminate_backend(pid) from pg_stat_activity where datname=?;"
            void $ PG.execute conn q [PostgresClient.oDbname options]
  case e of
    Left (_ :: IOError) -> pure () -- expected
    Right _ -> pure () -- Surprising ... but I do not know yet if this is a failure of termination or not.

-- | Stop the postgres process. This function attempts to the 'pidLock' before running.
--   'stopPostgres' will terminate all connections before shutting down postgres.
--   'stopPostgres' is useful for testing backup strategies.
stopPostgres :: PostgresProcess -> IO (Maybe ExitCode)
stopPostgres db@PostgresProcess{..} = withLock pidLock $ readIORef pid >>= \case
  Nothing -> pure Nothing
  Just pHandle -> do
    withProcessHandle pHandle (\case
          OpenHandle p   -> do
            -- try to terminate the connects first. If we can't terminate still
            -- keep shutting down
            terminateConnections db

            signalProcess sigINT p
          OpenExtHandle {} -> pure () -- TODO log windows is not supported
          ClosedHandle _ -> return ()
          )

    exitCode <- waitForProcess pHandle
    writeIORef pid Nothing
    pure $ Just exitCode

evaluatePostgresPlan
  :: CommonOptions -> PostgresPlan -> IO PostgresProcess
evaluatePostgresPlan common@CommonOptions {..} plan@PostgresPlan {..} = do
  options <- throwMaybe ClientCompleteOptions $ commonOptionsToConnectionOptions common
  let createDBResult = do
        thePid  <- evaluateProcess $ postgresPlanOptions
        pid     <- newIORef $ Just thePid
        pidLock <- newMVar ()

        let postgresInput = toPostgresInput plan
        pure PostgresProcess {..}

  commonOptionsLogger StartPostgres
  bracketOnError createDBResult stopPostgres $ \result -> do
    let checkForCrash = readIORef (pid result) >>= \case
          -- This should be impossible because we created the pid with a Just.
          Nothing -> throwIO $ StartPostgresDisappeared $ toPostgresInput plan
          -- Check the exit code on the pid
          Just thePid -> do
            mExitCode <- getProcessExitCode thePid
            for_ mExitCode (throwIO . StartPostgresFailed (toPostgresInput plan))

    commonOptionsLogger WaitForDB
    let connOpts = options
          { PostgresClient.oDbname = "template1"
          }
    waitForDB connOpts `race_` forever (checkForCrash >> threadDelay 100000)

    return result
-------------------------------------------------------------------------------
-- Plan
-------------------------------------------------------------------------------
data Plan = Plan
  { planCommonOptions :: PartialCommonOptions
  , planInitDb        :: Maybe PartialProcessOptions
  , planCreateDb      :: Maybe PartialProcessOptions
  , planPostgres      :: PartialPostgresPlan
  }
  deriving stock (Generic)
  deriving Semigroup via GenericSemigroup Plan
  deriving Monoid    via GenericMonoid Plan

data DB = DB
  { dbCommonInput     :: CommonInput
  , dbPostgresProcess :: PostgresProcess
  , dbInitDbInput     :: Maybe ProcessInput
  , dbCreateDbInput   :: Maybe ProcessInput
  }

addConnectionParams :: PostgresClient.Options -> ProcessOptions -> ProcessOptions
addConnectionParams = error "addConnectionParams"

initDbDefaultCommandLineOptions :: CommonOptions -> [String]
initDbDefaultCommandLineOptions CommonOptions {..} =
  let strArgs = (\(a,b) -> "--" <> a <> "=" <> b) <$>
        [ ("pgdata"  , commonOptionsDataDir)
        ]
  in "--nosync" : strArgs

defaultInitDbOptions :: CommonOptions -> IO PartialProcessOptions
defaultInitDbOptions commonOptions = do
  def <- standardProcessOptions
  pure $ def
    { partialProcessOptionsCmdLine = Replace $ initDbDefaultCommandLineOptions commonOptions
    , partialProcessOptionsName    = pure "initdb"
    }

executeInitDb :: CommonOptions -> PartialProcessOptions -> IO ProcessInput
executeInitDb commonOptions userOptions = do
  defs <- defaultInitDbOptions commonOptions
  completeOptions <- throwMaybe InitDbCompleteOptions $ completeProcessOptions $
    userOptions <> defs

  pure $ toProcessInput completeOptions

createDbDefaultCommandLineOptions :: CommonOptions -> [String]
createDbDefaultCommandLineOptions CommonOptions {..} =
  let strArgs = (\(a,b) -> a <> "=" <> b) <$>
        [ ("-h", socketClassToHost commonOptionsSocketClass)
        , ("-p", show commonOptionsPort)
        ]
  in strArgs <> [commonOptionsDbName]

defaultCreateDbOptions :: CommonOptions -> IO PartialProcessOptions
defaultCreateDbOptions commonOptions = do
  def <- standardProcessOptions
  pure $ def
    { partialProcessOptionsCmdLine = Replace $ createDbDefaultCommandLineOptions commonOptions
    , partialProcessOptionsName    = pure "createdb"
    }

executeCreateDb :: CommonOptions -> PartialProcessOptions -> IO ProcessInput
executeCreateDb commonOptions userOptions = do
  defs <- defaultCreateDbOptions commonOptions
  completeOptions <- throwMaybe CreateDbCompleteOptions $ completeProcessOptions $
    userOptions <> defs

  pure $ toProcessInput completeOptions

startWith :: Plan -> IO (Either StartError DB)
startWith Plan {..} = startPartialCommonOptions planCommonOptions $
  \commonOptions@CommonOptions {..} -> try $ do
    initDbOutput <- for planInitDb $ executeInitDb commonOptions
    postgresPlan <- throwMaybe PostgresCompleteOptions $ completePostgresPlan $
      planPostgres <> defaultPostgresPlan commonOptions
    bracketOnError (evaluatePostgresPlan commonOptions postgresPlan) stopPostgres $ \result -> do
      createDbOutput <- for planCreateDb $ executeCreateDb commonOptions

      pure $ DB
        { dbCommonInput     = commonOptionsToCommonInput commonOptions
        , dbPostgresProcess = result
        , dbInitDbInput     = initDbOutput
        , dbCreateDbInput   = createDbOutput
        }

start :: IO (Either StartError DB)
start = startWith mempty

-- | Send the SIGHUP signal to the postgres process to start a config reload
reloadConfig :: DB -> IO ()
reloadConfig DB {..} = do
  let PostgresProcess {..} = dbPostgresProcess
  mHandle <- readIORef pid
  for_ mHandle $ \theHandle -> do
    mPid <- getPid theHandle
    for_ mPid $ signalProcess sigHUP
