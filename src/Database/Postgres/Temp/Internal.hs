{-|
This module provides the high level functions that are re-exported
by @Database.Postgres.Temp@. Additionally it includes some
identifiers that are used for testing but are not exported.
-}
module Database.Postgres.Temp.Internal where
import Database.Postgres.Temp.Internal.Core
import Database.Postgres.Temp.Internal.Partial
import Control.Exception
import Control.Monad (void)
import qualified Database.PostgreSQL.Simple.Options as Client
import System.Exit (ExitCode(..))
import Data.ByteString (ByteString)
import Control.Monad.Trans.Cont
import qualified Database.PostgreSQL.Simple as PG
import qualified Data.Map.Strict as Map

-- | Handle for holding temporary resources, the @postgres@ process handle
--   and postgres connection information. The 'DB' also includes the
--   final 'Plan' that was used to start @initdb@, @createdb@ and
--   @postgres@. See 'toConnectionString' for converting a 'DB' to
--   postgresql connection string.
data DB = DB
  { dbResources :: Resources
  -- ^ Temporary resources and the final 'Plan'.
  , dbPostgresProcess :: PostgresProcess
  -- ^ @postgres@ process handle and the connection options.
  }

-- | Convert a 'DB' to a connection string. Alternatively one can access the
--   'Client.Options' using
--    @postgresProcessClientConfig . dbPostgresProcess@
toConnectionString :: DB -> ByteString
toConnectionString
  = Client.toConnectionString
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

{-|
The default configuration. This will create a database called \"postgres\"
   via @initdb@ (it's default behavior).
   It will create a temporary directory for the data and a temporary directory
 for a unix socket on a random port.
 Additionally it will use append the following onto the \"postgresql.conf\"

 @
   shared_buffers = 12MB
   fsync = off
   synchronous_commit = off
   full_page_writes = off
   log_min_duration_statement = 0
   log_connections = on
   log_disconnections = on
   client_min_messages = ERROR
@

'defaultConfig' also passes the @--no-sync@ flag to @initdb@.

If you would like to customize this behavior you can start with the
'defaultConfig' and overwrite fields or combine a 'defaultConfig' with another 'Config'
 using '<>' ('mappend').

 Alternatively you can eschew 'defaultConfig' altogether, however
 your @postgres@ might start and run faster if you use
 'defaultConfig'.

 'defaultConfig' also sets the 'partialPlanInitDb' to
  'pure' 'standardProcessConfig' and
  'partialPostgresPlanProcessConfig' to 'standardProcessConfig'.
-}
defaultConfig :: Config
defaultConfig = mempty
  { configPlan = mempty
    { partialPlanLogger = pure mempty
    , partialPlanConfig = defaultPostgresConfig
    , partialPlanCreateDb = Accum Nothing
    , partialPlanInitDb = pure standardProcessConfig
      { partialProcessConfigCmdLine = mempty
          { partialCommandLineArgsKeyBased = Map.singleton "--no-sync" Nothing
          }
      }
    , partialPlanPostgres = mempty
        { partialPostgresPlanProcessConfig = standardProcessConfig
        }
    }
  }

{-|
'mappend' the 'defaultConfig' with a 'Config' that provides additional
   \"postgresql.conf\" lines. Equivalent to

@
defaultPostgresConf extra = defaultConfig <> mempty
  { configPlan = mempty
    { partialPlanConfig = extra
    }
  }
@

-}
defaultPostgresConf :: [String] -> Config
defaultPostgresConf extra = defaultConfig <> mempty
  { configPlan = mempty
    { partialPlanConfig = extra
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
startWith extra = try $ evalContT $ do
  dbResources@Resources {..} <-
    ContT $ bracketOnError (initConfig extra) shutdownResources
  dbPostgresProcess <-
    ContT $ bracketOnError (initPlan resourcesPlan) stopPostgresProcess
  pure DB {..}

-- | Default start behavior. Equivalent to calling 'startWith' with the
--   'defaultConfig'
start :: IO (Either StartError DB)
start = startWith defaultConfig

-- | Stop the @postgres@ process and cleanup any temporary directories that
--   might have been created.
stop :: DB -> IO ()
stop DB {..} = do
  void $ stopPostgresProcess dbPostgresProcess
  shutdownResources dbResources

-- | Only stop the @postgres@ process but leave any temporary resources.
--   Useful for testing backup strategies when used in conjunction with
--   'restart' or 'withRestart'.
stopPostgres :: DB -> IO ExitCode
stopPostgres = stopPostgresProcess . dbPostgresProcess

-- | Restart the @postgres@ using the 'Plan' from the 'DB'
--  (e.g. @resourcesPlan . dbResources@)
restart :: DB -> IO (Either StartError DB)
restart db@DB{..} = try $ do
  void $ stopPostgres db
  let plan = resourcesPlan dbResources
  bracketOnError (startPostgresProcess (planLogger plan) $ planPostgres plan)
    stopPostgresProcess $ \result ->
      pure $ db { dbPostgresProcess = result }

-- | Reload the configuration file without shutting down. Calls
--   @pg_reload_conf()@.
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
with = withPlan defaultConfig

-- | Exception safe version of 'restart'
withRestart :: DB -> (DB -> IO a) -> IO (Either StartError a)
withRestart db f = bracket (restart db) (either mempty stop) $
  either (pure . Left) (fmap Right . f)

-- | Attempt to create a 'Config' from a 'Client.Options'. Useful if you
--   want to create a database owned by a specific user you will also login
--   with among other use cases.
optionsToDefaultConfig :: Client.Options -> Config
optionsToDefaultConfig opts@Client.Options {..} =
  let generated = optionsToConfig opts
      startingConfig =
        if partialPlanCreateDb (configPlan generated) == mempty
          then defaultConfig
          else setCreateDb defaultConfig $ pure standardProcessConfig
  in startingConfig <> generated

-- | Set a 'Config's 'partialPlanCreateDb' value.
setCreateDb :: Config -> Accum PartialProcessConfig -> Config
setCreateDb config@Config {..} new = config
  { configPlan = configPlan
      { partialPlanCreateDb = new
      }
  }

-- | Set a 'Config's 'partialPlanInitDb' value.
setInitDb :: Config -> Accum PartialProcessConfig -> Config
setInitDb config@Config {..} new = config
  { configPlan = configPlan
      { partialPlanInitDb = new
      }
  }