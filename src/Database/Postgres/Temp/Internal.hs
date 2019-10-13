{-# LANGUAGE RecordWildCards, LambdaCase, ScopedTypeVariables, DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
{-# LANGUAGE TupleSections, DerivingStrategies, DerivingVia #-}

module Database.Postgres.Temp.Internal where
import Database.Postgres.Temp.Etc

import Control.Concurrent (MVar, newMVar, threadDelay)
import Control.Concurrent.Async (race_)
import Control.Exception
import Control.Monad (forever, void)
import Data.Maybe
import qualified Data.ByteString.Char8 as BSC
import Data.Foldable (for_)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Typeable (Typeable)
import qualified Database.PostgreSQL.Simple as PG
import qualified Database.PostgreSQL.Simple.Options as PostgresClient
import GHC.Generics (Generic)
import Network.Socket.Free (getFreePort)
import System.Exit (ExitCode(..))
import System.Posix.Signals (sigHUP, sigINT, signalProcess)
import System.Process (getPid, getProcessExitCode, waitForProcess)
import System.Process.Internals
import Data.Traversable (for)
import Data.Monoid.Generic
import Control.Applicative
import qualified Database.PostgreSQL.Simple.PartialOptions as Client

-------------------------------------------------------------------------------
-- Events and Exceptions
--------------------------------------------------------------------------------
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
-- PartialCommonOptions
-------------------------------------------------------------------------------
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
-------------------------------------------------------------------------------
-- CommonOptions
-------------------------------------------------------------------------------
data CommonOptions = CommonOptions
  { commonOptionsDbName        :: String
  , commonOptionsDataDir       :: DirectoryType
  , commonOptionsPort          :: Int
  , commonOptionsSocketClass   :: SocketClass
  , commonOptionsClientOptions :: Client.PartialOptions
  , commonOptionsLogger        :: Event -> IO ()
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
-------------------------------------------------------------------------------
-- CommonInput
-------------------------------------------------------------------------------
data CommonInput = CommonInput
  { commonInputDbName      :: String
  , commonInputDataDir     :: DirectoryType
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

startPartialCommonOptions :: PartialCommonOptions -> (CommonOptions -> IO a) -> IO a
startPartialCommonOptions PartialCommonOptions {..} f = do
  commonOptionsPort <- maybe getFreePort pure partialCommonOptionsPort

  let commonOptionsDbName        = fromMaybe "test" partialCommonOptionsDbName
      commonOptionsLogger        = fromMaybe (const $ pure ()) partialCommonOptionsLogger
      commonOptionsClientOptions = partialCommonOptionsClientOptions

  bracketOnError (initializeDirectoryType "tmp-postgres-data" partialCommonOptionsDataDir) cleanupDirectoryType $
    \commonOptionsDataDir ->
      startPartialSocketClass partialCommonOptionsSocketClass $
        \commonOptionsSocketClass ->
          f CommonOptions {..}

stopCommonInput :: CommonInput -> IO ()
stopCommonInput CommonInput {..} = do
  stopSocketOptions commonInputSocketClass
  cleanupDirectoryType commonInputDataDir
-------------------------------------------------------------------------------
-- PartialPostgresPlan
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
-------------------------------------------------------------------------------
-- PostgresPlan
-------------------------------------------------------------------------------
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
-------------------------------------------------------------------------------
-- PostgresInput
-------------------------------------------------------------------------------
data PostgresInput = PostgresInput
  { postgresOptionsProcessOptions :: ProcessInput
  , postgresOptionsConfig  :: String
  } deriving (Show, Eq, Ord)

toPostgresInput :: PostgresPlan -> PostgresInput
toPostgresInput PostgresPlan {..} = PostgresInput
  { postgresOptionsProcessOptions = toProcessInput postgresPlanOptions
  , postgresOptionsConfig  = postgresPlanConfig
  }
-------------------------------------------------------------------------------
-- PostgresProcess
-------------------------------------------------------------------------------
data PostgresProcess = PostgresProcess
  { pidLock         :: MVar ()
  -- ^ A lock used internally to makes sure access to 'pid' is serialized
  , pid             :: IORef (Maybe ProcessHandle)
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
-------------------------------------------------------------------------------
-- DB
-------------------------------------------------------------------------------
data DB = DB
  { dbCommonInput     :: CommonInput
  , dbPostgresProcess :: PostgresProcess
  , dbInitDbInput     :: Maybe ProcessInput
  , dbCreateDbInput   :: Maybe ProcessInput
  }

-- | Send the SIGHUP signal to the postgres process to start a config reload
--   TODO use `reload_config()`. Maybe it makes it easier to simplify things
reloadConfig :: DB -> IO ()
reloadConfig DB {..} = do
  let PostgresProcess {..} = dbPostgresProcess
  mHandle <- readIORef pid
  for_ mHandle $ \theHandle -> do
    mPid <- getPid theHandle
    for_ mPid $ signalProcess sigHUP
-------------------------------------------------------------------------------
-- Starting
-------------------------------------------------------------------------------
defaultInitDbOptions :: CommonOptions -> IO PartialProcessOptions
defaultInitDbOptions CommonOptions {..} = do
  def <- standardProcessOptions
  pure $ def
    { partialProcessOptionsCmdLine = Replace $
        "--nosync" : ["--pgdata=" <> toFilePath commonOptionsDataDir]
    , partialProcessOptionsName    = pure "initdb"
    }

executeInitDb :: CommonOptions -> PartialProcessOptions -> IO ProcessInput
executeInitDb commonOptions userOptions = do
  defs <- defaultInitDbOptions commonOptions
  completeOptions <- throwMaybe InitDbCompleteOptions $ completeProcessOptions $
    userOptions <> defs

  pure $ toProcessInput completeOptions

defaultCreateDbOptions :: CommonOptions -> IO PartialProcessOptions
defaultCreateDbOptions CommonOptions {..} = do
  let strArgs = (\(a,b) -> a <> "=" <> b) <$>
        [ ("-h", socketClassToHost commonOptionsSocketClass)
        , ("-p", show commonOptionsPort)
        ]
  def <- standardProcessOptions
  pure $ def
    { partialProcessOptionsCmdLine = Replace $ strArgs <> [commonOptionsDbName]
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
-------------------------------------------------------------------------------
-- Stopping
-------------------------------------------------------------------------------
stop :: DB -> IO ()
stop DB {..} = do
  void $ stopPostgres dbPostgresProcess
  stopCommonInput dbCommonInput
-------------------------------------------------------------------------------
-- with
-------------------------------------------------------------------------------
withPlan :: Plan -> (DB -> IO a) -> IO (Either StartError a)
withPlan plan f = bracket (startWith plan) (either (const $ pure ()) stop) $
  either (pure . Left) (fmap Right . f)

with :: (DB -> IO a) -> IO (Either StartError a)
with = withPlan mempty
