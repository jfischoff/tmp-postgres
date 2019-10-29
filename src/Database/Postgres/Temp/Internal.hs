module Database.Postgres.Temp.Internal where
import Database.Postgres.Temp.Core
import Database.Postgres.Temp.Partial
import Control.Exception
import Control.Monad (void)
import qualified Database.PostgreSQL.Simple.Options as PostgresClient
import qualified Database.PostgreSQL.Simple.PartialOptions as Client
import System.Exit (ExitCode(..))
import Data.ByteString (ByteString)
import Control.Monad.Trans.Cont
-- TODO return stderr if there is an exception

data DB = DB
  { dbResources :: Resources
  , dbPostgresProcess :: PostgresProcess
  }

toConnectionString :: DB -> ByteString
toConnectionString
  = PostgresClient.toConnectionString
  . postgresProcessClientOptions
  . dbPostgresProcess
--------------------------------------------------
-- Life Cycle Management
-------------------------------------------------------------------------------
-- Default postgres options
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

defaultPartialResources :: IO PartialResources
defaultPartialResources = do
  theStandardProcessOptions <- standardProcessOptions
  pure mempty
    { partialResourcesPlan = mempty
      { partialPlanLogger = pure print
      , partialPlanConfig = Mappend defaultConfig
      , partialPlanCreateDb = Mappend $ Just $ theStandardProcessOptions
          { partialProcessOptionsCmdLine = Mappend ["test"]
          }
      , partialPlanInitDb = Mappend $ Just $ theStandardProcessOptions
        { partialProcessOptionsCmdLine = Mappend ["--no-sync"]
        }
      , partialPlanPostgres = mempty
          { partialPostgresPlanProcessOptions = theStandardProcessOptions
          , partialPostgresPlanClientOptions = mempty
            { Client.dbname = pure "test"
            }
          }
      }
    }

startWith :: PartialResources -> IO (Either StartError DB)
startWith x = try $ evalContT $ do
  dbResources@Resources {..} <- ContT $ bracketOnError (startPartialResources x) stopResources
  dbPostgresProcess <- ContT $ bracketOnError (startPlan resourcesPlan) stopPostgresProcess
  pure DB {..}

start :: IO (Either StartError DB)
start = startWith =<< defaultPartialResources
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
  bracketOnError (startPostgres (planLogger plan) $ planPostgres plan)
    stopPostgresProcess $ \result ->
      pure $ db { dbPostgresProcess = result }
-------------------------------------------------------------------------------
-- Exception safe interface
-------------------------------------------------------------------------------
withPlan :: PartialResources -> (DB -> IO a) -> IO (Either StartError a)
withPlan plan f = bracket (startWith plan) (either mempty stop) $
  either (pure . Left) (fmap Right . f)

with :: (DB -> IO a) -> IO (Either StartError a)
with f = do
  initialPlan <- defaultPartialResources
  withPlan initialPlan f

withRestart :: DB -> (DB -> IO a) -> IO (Either StartError a)
withRestart db f = bracket (restartPostgres db) (either mempty stop) $
  either (pure . Left) (fmap Right . f)

