module Database.Postgres.Temp.Internal where
import Database.Postgres.Temp.Core
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
-- TODO return stderr if there is an exception
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

type CommonOptions = GCommonOptions Event
-------------------------------------------------------------------------------
-- PartialCommonOptions
-------------------------------------------------------------------------------
-- TODO use last and derive the monoid
data PartialCommonOptions = PartialCommonOptions
  { partialCommonOptionsDataDir       :: Maybe FilePath
  , partialCommonOptionsSocketClass   :: PartialSocketClass
  , partialCommonOptionsLogger        :: Maybe (Event -> IO ())
  , partialCommonOptionsClientOptions :: Client.PartialOptions
  }
  deriving stock (Generic)

instance Semigroup PartialCommonOptions where
  x <> y = PartialCommonOptions
    { partialCommonOptionsDataDir     =
        partialCommonOptionsDataDir x <|> partialCommonOptionsDataDir y
    , partialCommonOptionsSocketClass =
        partialCommonOptionsSocketClass x <> partialCommonOptionsSocketClass y
    , partialCommonOptionsLogger      =
        partialCommonOptionsLogger x <|> partialCommonOptionsLogger y
    , partialCommonOptionsClientOptions =
        partialCommonOptionsClientOptions x <> partialCommonOptionsClientOptions y
    }

instance Monoid PartialCommonOptions where
  mempty = PartialCommonOptions
    { partialCommonOptionsDataDir       = Nothing
    , partialCommonOptionsSocketClass   = mempty
    , partialCommonOptionsLogger        = Nothing
    , partialCommonOptionsClientOptions = mempty
    }

-------------------------------------------------------------------------------
-- CommonOptions life cycle
-------------------------------------------------------------------------------
startPartialCommonOptions
  :: PartialCommonOptions -> (CommonOptions -> IO a) -> IO a
startPartialCommonOptions PartialCommonOptions {..} f = do
  port <- maybe getFreePort pure $ getLast $
    Client.port partialCommonOptionsClientOptions

  let dbName = fromMaybe "test" $ getLast $
        Client.dbname partialCommonOptionsClientOptions

      commonOptionsLogger        = fromMaybe mempty partialCommonOptionsLogger

      initCommon = initializeDirectoryType "tmp-postgres-data"
        partialCommonOptionsDataDir

  bracketOnError initCommon cleanupDirectoryType $ \commonOptionsDataDir ->
      startPartialSocketClass partialCommonOptionsSocketClass $
        \commonOptionsSocketClass -> do
          let host = socketClassToHost commonOptionsSocketClass
              defaultClientOptions = mempty
                { Client.port = pure port
                , Client.dbname = pure dbName
                , Client.host = pure host
                }
          commonOptionsClientOptions <- throwMaybe ClientCompleteOptions .
              either (const Nothing) Just $
                Client.completeOptions $
                  partialCommonOptionsClientOptions <> defaultClientOptions
          f CommonOptions {..}

stopCommonOptions :: CommonOptions -> IO ()
stopCommonOptions CommonOptions {..} = do
  stopSocketOptions commonOptionsSocketClass
  cleanupDirectoryType commonOptionsDataDir
-------------------------------------------------------------------------------
-- PartialPostgresPlan
-------------------------------------------------------------------------------
data PartialPostgresPlan = PartialPostgresPlan
  { partialPostgresPlanConfig  :: Lastoid [String]
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
    { partialPostgresPlanConfig  = Replace $
        defaultConfig <> listenAddressConfig commonOptionsSocketClass
    , partialPostgresPlanOptions = processOptions
        { partialProcessOptionsName = pure "postgres"
        , partialProcessOptionsCmdLine = Replace
            [ "-D" <> toFilePath commonOptionsDataDir
            , "-p" <> show (fromJust $ PostgresClient.oPort commonOptionsClientOptions)
            ]
        }
    }


completePostgresPlan :: PartialPostgresPlan -> Maybe PostgresPlan
completePostgresPlan PartialPostgresPlan {..} = do
  postgresPlanConfig <- case partialPostgresPlanConfig of
    Mappend _ -> Nothing
    Replace x -> Just $ unlines x

  postgresPlanOptions <- completeProcessOptions partialPostgresPlanOptions

  pure PostgresPlan {..}


-------------------------------------------------------------------------------
-- PartialPlan
-------------------------------------------------------------------------------
data PartialPlan = PartialPlan
  { planCommonOptions :: PartialCommonOptions
  , planInitDb        :: Last (Maybe PartialProcessOptions)
  , planCreateDb      :: Last (Maybe PartialProcessOptions)
  , planPostgres      :: PartialPostgresPlan
  }
  deriving stock (Generic)
  deriving Semigroup via GenericSemigroup PartialPlan
  deriving Monoid    via GenericMonoid PartialPlan
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
        , ("-p", show (fromJust $ PostgresClient.oPort commonOptionsClientOptions))
        ]
  def <- standardProcessOptions
  pure $ def
    { partialProcessOptionsCmdLine = Replace $ strArgs <> [PostgresClient.oDbname commonOptionsClientOptions]
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
startWith :: PartialPlan -> IO (Either StartError DB)
startWith PartialPlan {..} = try $ startPartialCommonOptions planCommonOptions $
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
withPlan :: PartialPlan -> (DB -> IO a) -> IO (Either StartError a)
withPlan plan f = bracket (startWith plan) (either mempty stop) $
  either (pure . Left) (fmap Right . f)

with :: (DB -> IO a) -> IO (Either StartError a)
with = withPlan mempty

withRestart :: DB -> (DB -> IO a) -> IO (Either StartError a)
withRestart db f = bracket (restartPostgres db) (either mempty stop) $
  either (pure . Left) (fmap Right . f)
