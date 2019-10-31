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

-- | Convert a 'DB' to a connection string. Alternatively one can access the
--   'PostgresClient.Options' using
--    @postgresProcessClientConfig . dbPostgresProcess@
toConnectionString :: DB -> ByteString
toConnectionString
  = PostgresClient.toConnectionString
  . postgresProcessClientConfig
  . dbPostgresProcess
-------------------------------------------------------------------------------
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
--   If you would like complete control over the behavior of @initdb@,
--   @postgres@ and @createdb@ you can call the internal function 'initPlan'
--   directly although this should not be necessary.
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

-- | Create temporary resources and use them to make a 'Config'.
--   The generated 'Config' is combined with the passed in @extraConfiguration@
--   to create a 'Plan' that is used to create a database.
--   The output 'DB' includes references to the temporary resources for
--   cleanup and the final plan that was used to generate the database and
--   processes
startWith :: Config
          -- ^ @extraConfiguration@ that is 'mappend'ed to the generated `Config`.
          -- The extra config is 'mappend'ed second, e.g.
          -- @generatedConfig <> extraConfiguration@
          -> IO (Either StartError DB)
startWith x = try $ evalContT $ do
  dbResources@Resources {..} <-
    ContT $ bracketOnError (initConfig x) shutdownResources
  dbPostgresProcess <-
    ContT $ bracketOnError (initPlan resourcesPlan) stopPostgresProcess
  pure DB {..}

-- | Default start behavior. Equivalent to calling 'startWith' with the
--   'defaultConfig'
start :: IO (Either StartError DB)
start = startWith =<< defaultConfig
-------------------------------------------------------------------------------
-- Stopping
-------------------------------------------------------------------------------
-- | Stop the @postgres@ process and cleanup any temporary directories that
--   might have been created.
stop :: DB -> IO ()
stop DB {..} = do
  void $ stopPostgresProcess dbPostgresProcess
  shutdownResources dbResources
-------------------------------------------------------------------------------
-- stopPostgres
-------------------------------------------------------------------------------
stopPostgres :: DB -> IO ExitCode
stopPostgres = stopPostgresProcess . dbPostgresProcess
-------------------------------------------------------------------------------
-- restart
-------------------------------------------------------------------------------
restart :: DB -> IO (Either StartError DB)
restart db@DB{..} = try $ do
  void $ stopPostgres db
  let plan = resourcesPlan dbResources
  bracketOnError (startPostgresProcess (planLogger plan) $ planPostgres plan)
    stopPostgresProcess $ \result ->
      pure $ db { dbPostgresProcess = result }
-------------------------------------------------------------------------------
-- reload
-------------------------------------------------------------------------------
reloadConfig :: DB -> IO ()
reloadConfig db =
  bracket (PG.connectPostgreSQL $ toConnectionString db) PG.close $ \conn -> do
    (void :: IO [PG.Only Bool] -> IO ()) $
      PG.query_ conn "SELECT pg_reload_conf()"
-------------------------------------------------------------------------------
-- Exception safe interface
-------------------------------------------------------------------------------
-- | Exception safe default database create. Takes an @action@ continuation
--   which is given a 'DB' it can use to connect
--   to (see 'toConnectionString' or 'postgresProcessClientConfig').
--   All of the database resources are automatically cleaned up on
--   completion even in the face of exceptions.
withPlan :: Config
         -- ^ @extraConfiguration@. Combined with the generated 'Config'. See
         -- 'startWith' for more info
         -> (DB -> IO a)
         -- ^ @action@ continuation
         -> IO (Either StartError a)
withPlan plan f = bracket (startWith plan) (either mempty stop) $
  either (pure . Left) (fmap Right . f)

-- | Default expectation safe interface. Equivalent to 'withPlan' the
--   'defaultConfig'
with :: (DB -> IO a)
     -- ^ @action@ continuation.
     -> IO (Either StartError a)
with f = do
  initialPlan <- defaultConfig
  withPlan initialPlan f

-- | Exception safe version of 'restart'
withRestart :: DB -> (DB -> IO a) -> IO (Either StartError a)
withRestart db f = bracket (restart db) (either mempty stop) $
  either (pure . Left) (fmap Right . f)
