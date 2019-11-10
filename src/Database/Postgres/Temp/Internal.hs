{-|
This module provides the high level functions that are re-exported
by @Database.Postgres.Temp@. Additionally it includes some
identifiers that are used for testing but are not exported.
-}
module Database.Postgres.Temp.Internal where

import Database.Postgres.Temp.Internal.Core
import Database.Postgres.Temp.Internal.Partial

import           Control.Exception
import           Control.Monad (void)
import           Control.Monad.Trans.Cont
import           Data.ByteString (ByteString)
import qualified Data.Map.Strict as Map
import qualified Database.PostgreSQL.Simple as PG
import qualified Database.PostgreSQL.Simple.Options as Client
import           System.Exit (ExitCode(..))
import           Text.PrettyPrint.ANSI.Leijen hiding ((<$>))

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

instance Pretty DB where
  pretty DB {..}
    =  text "dbResources"
    <> softline
    <> indent 2 (pretty dbResources)
    <> hardline
    <> text "dbPostgresProcess"
    <> softline
    <> indent 2 (pretty dbPostgresProcess)

-- | Convert a 'DB' to a connection string. Alternatively one can access the
--   'Client.Options' using 'toConnectionOptions'
toConnectionString :: DB -> ByteString
toConnectionString
  = Client.toConnectionString
  . toConnectionOptions

-- | Convert a 'DB' to a connection 'Client.Options' type.
toConnectionOptions :: DB -> Client.Options
toConnectionOptions
  = postgresProcessClientOptions
  . dbPostgresProcess

-- | Access the data directory. This was either generated or
--   specified explicitly when creating the 'Config'
toDataDirectory :: DB -> FilePath
toDataDirectory =  toFilePath . resourcesDataDir . dbResources
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
 which is optimized for performance.

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

To append additional lines to \"postgresql.conf\" file create a
custom 'Config' like the following.

 @
  custom = defaultConfig <> mempty
    { configPlan = mempty
      { partialPlanConfig =
          [ "wal_level = replica"
          , "archive_mode = on"
          , "max_wal_senders = 2"
          , "fsync = on"
          , "synchronous_commit = on"
          ]
      }
    }
 @

Or using the provided lenses and your favorite lens library

 @
  custom = defaultConfig & 'configPlanL' . 'partialPlanConfigL' <>~
    [ "wal_level = replica"
    , "archive_mode = on"
    , "max_wal_senders = 2"
    , "fsync = on"
    , "synchronous_commit = on"
    ]
 @

 This is common enough there is `defaultPostgresConf` which
 is a helper to do this.

 As an alternative to using 'defaultConfig' one could create a
 config from connections parameters using 'optionsToDefaultConfig'
-}
defaultConfig :: Config
defaultConfig = mempty
  { configPlan = mempty
    { partialPlanLogger = pure mempty
    , partialPlanConfig = defaultPostgresConfig
    , partialPlanCreateDb = Nothing
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
startConfig :: Config
          -- ^ @extraConfiguration@ that is 'mappend'ed to the generated `Config`.
          -- The extra config is 'mappend'ed second, e.g.
          -- @generatedConfig <> extraConfiguration@
          -> IO (Either StartError DB)
startConfig extra = try $ evalContT $ do
  dbResources@Resources {..} <-
    ContT $ bracketOnError (setupConfig extra) cleanupConfig
  dbPostgresProcess <-
    ContT $ bracketOnError (startPlan resourcesPlan) stopPostgresProcess
  pure DB {..}

-- | Default start behavior. Equivalent to calling 'startConfig' with the
--   'defaultConfig'
start :: IO (Either StartError DB)
start = startConfig defaultConfig

-- | Stop the @postgres@ process and cleanup any temporary directories that
--   might have been created.
stop :: DB -> IO ()
stop DB {..} = do
  void $ stopPostgresProcess dbPostgresProcess
  cleanupConfig dbResources

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
  bracket (PG.connectPostgreSQL $ toConnectionString db) PG.close $ \conn ->
    (void :: IO [PG.Only Bool] -> IO ()) $
      PG.query_ conn "SELECT pg_reload_conf()"
-------------------------------------------------------------------------------
-- Exception safe interface
-------------------------------------------------------------------------------
{-|
Exception safe default database create. Takes an @action@ continuation
which is given a 'DB' it can use to connect
to (see 'toConnectionString' or 'postgresProcessClientOptions').
All of the database resources are automatically cleaned up on
completion even in the face of exceptions.
Based on the value of 'configSocket' a \"postgresql.conf\" is created with

 @
   listen_addresses = \'IP_ADDRESS\'
 @

 if it is 'IpSocket'. If is 'UnixSocket' then the lines

 @
   listen_addresses = ''
   unix_socket_directories = SOCKET_DIRECTORY
 @

are added. This occurs as a side effect of calling 'withConfig'.
-}
withConfig :: Config
         -- ^ @extraConfiguration@. Combined with the generated 'Config'. See
         -- 'startConfig' for more info
         -> (DB -> IO a)
         -- ^ @action@ continuation
         -> IO (Either StartError a)
withConfig plan f = bracket (startConfig plan) (either mempty stop) $
  either (pure . Left) (fmap Right . f)

-- | Default expectation safe interface. Equivalent to 'withConfig' the
--   'defaultConfig'
with :: (DB -> IO a)
     -- ^ @action@ continuation.
     -> IO (Either StartError a)
with = withConfig defaultConfig

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
          else defaultConfig <> mempty
            { configPlan = mempty
              { partialPlanCreateDb = pure standardProcessConfig
              }
            }
  in startingConfig <> generated

-------------------------------------------------------------------------------
-- Pretty Printing
-------------------------------------------------------------------------------
-- | Display a 'Config'.
prettyPrintConfig :: Config -> String
prettyPrintConfig = show . pretty

-- | Display a 'DB'
prettyPrintDB :: DB -> String
prettyPrintDB = show . pretty
