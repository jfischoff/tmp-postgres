module Database.Postgres.Temp.Internal where
import Database.Postgres.Temp.Etc
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (race_)
import Control.Exception
import Control.Monad (forever, void)
import Data.Maybe
import Data.Foldable (for_)
import Data.Typeable (Typeable)
import qualified Database.PostgreSQL.Simple as PG
import qualified Database.PostgreSQL.Simple.Options as PostgresClient
import GHC.Generics (Generic)
import Network.Socket.Free (getFreePort)
import System.Exit (ExitCode(..))
import System.Posix.Signals (sigINT, signalProcess)
import System.Process (getProcessExitCode, waitForProcess)
import System.Process.Internals
import Data.Monoid.Generic
import Control.Applicative
import qualified Database.PostgreSQL.Simple.PartialOptions as Client
import Data.String
import Data.Monoid
import Data.ByteString (ByteString)
import System.Directory
-------------------------------------------------------------------------------
-- Events and Exceptions
--------------------------------------------------------------------------------
data StartError
  = InitDBFailed   ExitCode
  | CreateDBFailed [String] ExitCode
  | StartPostgresFailed ExitCode
  | StartPostgresDisappeared
  | InitDbCompleteOptions
  | CreateDbCompleteOptions
  | PostgresCompleteOptions
  | ClientCompleteOptions
  | InitDbNotFound
  | CreateDbNotFound
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
--TODO remove DbName and Port
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
-- CommonOptions life cycle
-------------------------------------------------------------------------------
startPartialCommonOptions
  :: PartialCommonOptions -> (CommonOptions -> IO a) -> IO a
startPartialCommonOptions PartialCommonOptions {..} f = do
  commonOptionsPort <- maybe getFreePort pure partialCommonOptionsPort

  let commonOptionsDbName        = fromMaybe "test" partialCommonOptionsDbName
      commonOptionsLogger        = fromMaybe mempty partialCommonOptionsLogger
      commonOptionsClientOptions = partialCommonOptionsClientOptions

      initCommon = initializeDirectoryType "tmp-postgres-data"
        partialCommonOptionsDataDir

  bracketOnError initCommon cleanupDirectoryType $ \commonOptionsDataDir ->
      startPartialSocketClass partialCommonOptionsSocketClass $
        \commonOptionsSocketClass -> f CommonOptions {..}

stopCommonOptions :: CommonOptions -> IO ()
stopCommonOptions CommonOptions {..} = do
  stopSocketOptions commonOptionsSocketClass
  cleanupDirectoryType commonOptionsDataDir
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

defaultPostgresPlan :: CommonOptions -> IO PartialPostgresPlan
defaultPostgresPlan CommonOptions {..} = do
  processOptions <- standardProcessOptions
  pure $ PartialPostgresPlan
    { partialPostgresPlanConfig  = Replace $ unlines $
        defaultConfig <> listenAddressConfig commonOptionsSocketClass
    , partialPostgresPlanOptions = processOptions
        { partialProcessOptionsName = pure "postgres"
        , partialProcessOptionsCmdLine = Replace
            [ "-D" <> toFilePath commonOptionsDataDir
            , "-p" <> show commonOptionsPort
            ]
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
-- PostgresProcess
-------------------------------------------------------------------------------
data PostgresProcess = PostgresProcess
  { pid          :: ProcessHandle
  -- ^ The process handle for the @postgres@ process.
  , options      :: PostgresClient.Options
  , postgresPlan :: PostgresPlan
  }
-------------------------------------------------------------------------------
-- PostgresProcess Life cycle management
-------------------------------------------------------------------------------
-- | Force all connections to the database to close. Can be useful in some
--   testing situations.
--   Called during shutdown as well.
terminateConnections :: PostgresProcess -> IO ()
terminateConnections PostgresProcess {..} = do
  let theConnectionString = PostgresClient.toConnectionString $ options
      terminationQuery = fromString $ unlines
        [ "SELECT pg_terminate_backend(pid)"
        , "FROM pg_stat_activity"
        , "WHERE datname=?;"
        ]
  e <- try $ bracket (PG.connectPostgreSQL theConnectionString) PG.close $
    \conn -> PG.execute conn terminationQuery [PostgresClient.oDbname options]
  case e of
    Left (_ :: IOError) -> pure ()
    Right _ -> pure ()

-- | Stop the postgres process. This function attempts to the 'pidLock' before running.
--   'stopPostgres' will terminate all connections before shutting down postgres.
--   'stopPostgres' is useful for testing backup strategies.
stopPostgresProcess :: PostgresProcess -> IO ExitCode
stopPostgresProcess db@PostgresProcess{..} = do
  withProcessHandle pid (\case
        OpenHandle p   -> do
          -- try to terminate the connects first. If we can't terminate still
          -- keep shutting down
          terminateConnections db

          signalProcess sigINT p
        OpenExtHandle {} -> pure () -- TODO log windows is not supported
        ClosedHandle _ -> return ()
        )

  exitCode <- waitForProcess pid
  pure exitCode

startPostgres
  :: CommonOptions -> PostgresPlan -> IO PostgresProcess
startPostgres common@CommonOptions {..} plan@PostgresPlan {..} = do
  writeFile (toFilePath commonOptionsDataDir <> "/postgresql.conf") postgresPlanConfig
  options <- throwMaybe ClientCompleteOptions $
    commonOptionsToConnectionOptions common
  let createDBResult = do
        pid  <- evaluateProcess $ postgresPlanOptions

        let postgresPlan = plan
        pure PostgresProcess {..}

  commonOptionsLogger StartPostgres
  bracketOnError createDBResult stopPostgresProcess $ \result -> do
    let checkForCrash = do
            mExitCode <- getProcessExitCode $ pid result
            for_ mExitCode (throwIO . StartPostgresFailed)

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
  , planInitDb        :: Last (Maybe PartialProcessOptions)
  , planCreateDb      :: Last (Maybe PartialProcessOptions)
  , planPostgres      :: PartialPostgresPlan
  }
  deriving stock (Generic)
  deriving Semigroup via GenericSemigroup Plan
  deriving Monoid    via GenericMonoid Plan
-------------------------------------------------------------------------------
-- DB
-------------------------------------------------------------------------------
data DB = DB
  { dbCommonOptions   :: CommonOptions
  , dbPostgresProcess :: PostgresProcess
  , dbInitDbInput     :: Maybe ProcessOptions
  , dbCreateDbInput   :: Maybe ProcessOptions
  , dbPostgresPlan    :: PostgresPlan
  }

toConnectionString :: DB -> ByteString
toConnectionString = PostgresClient.toConnectionString  . options . dbPostgresProcess

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

executeInitDb :: CommonOptions -> PartialProcessOptions -> IO ProcessOptions
executeInitDb commonOptions userOptions = do
  void $ throwMaybe InitDbNotFound =<< findExecutable "initdb"
  defs <- defaultInitDbOptions commonOptions
  completeOptions <- throwMaybe InitDbCompleteOptions $ completeProcessOptions $
    userOptions <> defs

  throwIfNotSuccess =<< executeProcess completeOptions

  pure completeOptions

defaultCreateDbOptions :: CommonOptions -> IO PartialProcessOptions
defaultCreateDbOptions CommonOptions {..} = do
  let strArgs = (\(a,b) -> a <> b) <$>
        [ ("-h", socketClassToHost commonOptionsSocketClass)
        , ("-p", show commonOptionsPort)
        ]
  def <- standardProcessOptions
  pure $ def
    { partialProcessOptionsCmdLine = Replace $ strArgs <> [commonOptionsDbName]
    , partialProcessOptionsName    = pure "createdb"
    }

executeCreateDb :: CommonOptions -> PartialProcessOptions -> IO ProcessOptions
executeCreateDb commonOptions userOptions = do
  void $ throwMaybe CreateDbNotFound =<< findExecutable "createdb"
  defs <- defaultCreateDbOptions commonOptions
  completeOptions <- throwMaybe CreateDbCompleteOptions $
    completeProcessOptions $ userOptions <> defs

  throwIfNotSuccess =<< executeProcess completeOptions

  pure completeOptions
-------------------------------------------------------------------------------
-- Life Cycle Management
-------------------------------------------------------------------------------
startWith :: Plan -> IO (Either StartError DB)
startWith Plan {..} = try $ startPartialCommonOptions planCommonOptions $
    \dbCommonOptions@CommonOptions {..} -> do
      dbInitDbInput <- case getLast planInitDb of
        Nothing      -> Just <$> executeInitDb dbCommonOptions mempty
        Just Nothing -> pure Nothing
        Just (Just x)       -> Just <$> executeInitDb dbCommonOptions x
      dbPostgresPlan <- throwMaybe PostgresCompleteOptions
        . completePostgresPlan
        . mappend planPostgres
        =<< defaultPostgresPlan dbCommonOptions

      let postgresStart = startPostgres dbCommonOptions dbPostgresPlan
      bracketOnError postgresStart stopPostgresProcess $ \dbPostgresProcess -> do
        dbCreateDbInput <- case getLast planCreateDb of
          Nothing      -> Just <$> executeCreateDb dbCommonOptions mempty
          Just Nothing -> pure Nothing
          Just (Just x)       -> Just <$> executeCreateDb dbCommonOptions x
        pure DB {..}

start :: IO (Either StartError DB)
start = startWith mempty
-------------------------------------------------------------------------------
-- Stopping
-------------------------------------------------------------------------------
stop :: DB -> IO ()
stop DB {..} = do
  void $ stopPostgresProcess dbPostgresProcess
  stopCommonOptions dbCommonOptions
-------------------------------------------------------------------------------
-- stopPostgres
-------------------------------------------------------------------------------
stopPostgres :: DB -> IO ExitCode
stopPostgres = stopPostgresProcess . dbPostgresProcess
-------------------------------------------------------------------------------
-- restart
-------------------------------------------------------------------------------
restartPostgres :: DB -> IO (Either StartError DB)
restartPostgres db@DB{..} = try $ do
  void $ stopPostgres db
  bracketOnError (startPostgres dbCommonOptions dbPostgresPlan)
    stopPostgresProcess $ \result ->
      pure $ db { dbPostgresProcess = result }
-------------------------------------------------------------------------------
-- Exception safe interface
-------------------------------------------------------------------------------
withPlan :: Plan -> (DB -> IO a) -> IO (Either StartError a)
withPlan plan f = bracket (startWith plan) (either mempty stop) $
  either (pure . Left) (fmap Right . f)

with :: (DB -> IO a) -> IO (Either StartError a)
with = withPlan mempty

withRestart :: DB -> (DB -> IO a) -> IO (Either StartError a)
withRestart db f = bracket (restartPostgres db) (either mempty stop) $
  either (pure . Left) (fmap Right . f)