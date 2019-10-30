module Database.Postgres.Temp.Internal where
import Database.Postgres.Temp.Internal.Core
import Database.Postgres.Temp.Internal.Partial
import Control.Exception
import Control.Monad (void)
import qualified Database.PostgreSQL.Simple.Options as PostgresClient
import qualified Database.PostgreSQL.Simple.PartialOptions as Client
import System.Exit (ExitCode(..))
import Data.ByteString (ByteString)
import Control.Monad.Trans.Cont
import qualified Database.PostgreSQL.Simple as PG

-- Need to add helper accessors for the DB to get the nested stuff out
-- General formatting cleanup
-- Fixup main module
-- Add plan pretty printing functions

data DB = DB
  { dbResources :: Resources
  , dbPostgresProcess :: PostgresProcess
  }

toConnectionString :: DB -> ByteString
toConnectionString
  = PostgresClient.toConnectionString
  . postgresProcessClientConfig
  . dbPostgresProcess
--------------------------------------------------
-- Life Cycle Management
-------------------------------------------------------------------------------
-- | Default postgres options
defaultPostgresConfig :: [String]
defaultPostgresConfig =
  [ "shared_buffers = 12MB"
  , "fsync = off"
  , "synchronous_commit = off"
  , "full_page_writes = off"
  , "log_min_duration_statement = 0"
  , "log_connections = on"
  , "log_disconnections = on"
  , "client_min_messages = ERROR"
  ]

-- | The default configuration. This will create a database called \"test\"
--   and create a temporary directory for the data and a temporary directory
--   for a unix socket on a random port.
--   If you would like to customize this behavior you can start with the
--   'defaultConfig' and overwrite fields or combine the 'Config' with another
--   config using '<>' ('mappend').
defaultConfig :: IO Config
defaultConfig = do
  theStandardProcessConfig <- standardProcessConfig
  pure mempty
    { configPlan = mempty
      { partialPlanLogger = pure print
      , partialPlanConfig = Mappend defaultPostgresConfig
      , partialPlanCreateDb = Mappend $ Just $ theStandardProcessConfig
          { partialProcessConfigCmdLine = Mappend ["test"]
          }
      , partialPlanInitDb = Mappend $ Just $ theStandardProcessConfig
        { partialProcessConfigCmdLine = Mappend ["--no-sync"]
        }
      , partialPlanPostgres = mempty
          { partialPostgresPlanProcessConfig = theStandardProcessConfig
          , partialPostgresPlanClientConfig = mempty
            { Client.dbname = pure "test"
            }
          }
      }
    }

startWith :: Config -> IO (Either StartError DB)
startWith x = try $ evalContT $ do
  dbResources@Resources {..} <- ContT $ bracketOnError (startConfig x) stopResources
  dbPostgresProcess <- ContT $ bracketOnError (startPlan resourcesPlan) stopPostgresProcess
  pure DB {..}

start :: IO (Either StartError DB)
start = startWith =<< defaultConfig
-------------------------------------------------------------------------------
-- Stopping
-------------------------------------------------------------------------------
stop :: DB -> IO ()
stop DB {..} = do
  void $ stopPostgresProcess dbPostgresProcess
  stopResources dbResources
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
  let plan = resourcesPlan dbResources
  bracketOnError (startPostgresProcess (planLogger plan) $ planPostgres plan)
    stopPostgresProcess $ \result ->
      pure $ db { dbPostgresProcess = result }

reloadConfig :: DB -> IO ()
reloadConfig db =
  bracket (PG.connectPostgreSQL $ toConnectionString db) PG.close $ \conn -> do
    (void :: IO [PG.Only Bool] -> IO ()) $ PG.query_ conn "SELECT pg_reload_conf()"
-------------------------------------------------------------------------------
-- Exception safe interface
-------------------------------------------------------------------------------
withPlan :: Config -> (DB -> IO a) -> IO (Either StartError a)
withPlan plan f = bracket (startWith plan) (either mempty stop) $
  either (pure . Left) (fmap Right . f)

with :: (DB -> IO a) -> IO (Either StartError a)
with f = do
  initialPlan <- defaultConfig
  withPlan initialPlan f

withRestart :: DB -> (DB -> IO a) -> IO (Either StartError a)
withRestart db f = bracket (restartPostgres db) (either mempty stop) $
  either (pure . Left) (fmap Right . f)
