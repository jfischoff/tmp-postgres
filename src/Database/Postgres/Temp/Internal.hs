{-|
This module provides the high level functions that are re-exported
by @Database.Postgres.Temp@. Additionally it includes some
identifiers that are used for testing but are not exported.
-}
module Database.Postgres.Temp.Internal where

import Database.Postgres.Temp.Internal.Core
import Database.Postgres.Temp.Internal.Config

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
--   final plan used to start @initdb@, @createdb@ and
--   @postgres@. See 'toConnectionString' or 'toConnectionOptions'
--   for converting a 'DB' to postgresql connection string.
data DB = DB
  { dbResources :: Resources
  -- ^ Temporary resources and the final 'CompletePlan'.
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

{-|
Make the data directory permanent. Useful for debugging.
If you are using 'with' or 'withConfig' this function will
not modify the 'DB' that is passed for cleanup. You will
need to setup your own bracket like

 @
    bracket (fmap 'makeDataDirPermanent' 'start') (either mempty 'stop')
 @

-}
makeDataDirPermanent :: DB -> DB
makeDataDirPermanent db = db
  { dbResources = makeResourcesDataDirPermanent $ dbResources db
  }

-- | Get the directory that is used to create other temporary directories
toTemporaryDirectory :: DB -> FilePath
toTemporaryDirectory = resourcesTemporaryDir . dbResources
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
 Additionally it will use the following \"postgresql.conf\"
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

'defaultConfig' also sets the 'initDbConfig' to
'pure' 'standardProcessConfig' and
'postgresConfig' to 'standardProcessConfig'.

To append additional lines to \"postgresql.conf\" file create a
custom 'Config' like the following.

 @
  custom = defaultConfig <> mempty
    { plan = mempty
      { postgresConfigFile =
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
  custom = defaultConfig & 'planL' . 'postgresConfigFile' <>~
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
  { plan = mempty
    { logger = pure mempty
    , postgresConfigFile = defaultPostgresConfig
    , createDbConfig = Nothing
    , initDbConfig = pure standardProcessConfig
      { commandLine = mempty
          { keyBased = Map.singleton "--no-sync" Nothing
          }
      }
    , postgresPlan = mempty
        { postgresConfig = standardProcessConfig
        }
    }
  }

{-|
'mappend' the 'defaultConfig' with a 'Config' that provides additional
   \"postgresql.conf\" lines. Equivalent to

@
defaultPostgresConf extra = defaultConfig <> mempty
  { plan = mempty
    { postgresConfigFile = extra
    }
  }
@

or with lenses

@
defaultPostgresConf extra = defaultConfig & planL . postgresConfigFile <>~ extra
@

-}
defaultPostgresConf :: [String] -> Config
defaultPostgresConf extra = defaultConfig <> mempty
  { plan = mempty
    { postgresConfigFile = extra
    }
  }

{-|

Create zero or more temporary resources and use them to make a 'Config'.

The passed in config is inspected and a generated config is created.
The final config is built by

 @
   generated '<>' extra
 @

Returns a 'DB' that requires cleanup. `startConfig` should be
used with a `bracket` and 'stop', e.g.

 @
   `withConfig` :: `Config` -> (`DB` -> IO a) -> IO (Either `StartError` a)
   'withConfig' plan f = `bracket` (`startConfig` plan) (either mempty `stop`) $
      either (pure . Left) (fmap Right . f)
 @

or just use 'withConfig'. If you are calling 'startConfig' you
probably want 'withConfig' anyway.

Based on the value of 'socketClass' a \"postgresql.conf\" is created with

 @
   listen_addresses = \'IP_ADDRESS\'
 @

 if it is 'IpSocket'. If is 'UnixSocket' then the lines

 @
   listen_addresses = ''
   unix_socket_directories = \'SOCKET_DIRECTORY\'
 @

are added.

-}
startConfig :: Config
          -- ^ @extra@ configuration that is 'mappend'ed last to the generated `Config`.
          -- @generated <> extra@
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

-- | Stop the @postgres@ process and cleanup any temporary resources that
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

-- | Restart the @postgres@ from 'DB' using the prior 'Plan'
restart :: DB -> IO (Either StartError DB)
restart db@DB{..} = try $ do
  void $ stopPostgres db
  let plan = resourcesPlan dbResources
  bracketOnError (startPostgresProcess (completePlanLogger plan) $ completePlanPostgres plan)
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
Exception safe database create with options. See 'startConfig' for more
details. Calls 'stop' even in the face of exceptions.
-}
withConfig :: Config
         -- ^ @extra@. 'Config' combined with the generated 'Config'. See
         -- 'startConfig' for more info
         -> (DB -> IO a)
         -- ^ @action@ continuation
         -> IO (Either StartError a)
withConfig extra f = bracket (startConfig extra) (either mempty stop) $
  either (pure . Left) (fmap Right . f)

{-| Default expectation safe interface. Equivalent to

 @
   'with' = 'withConfig' 'defaultConfig'
 @

-}
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
        if createDbConfig (plan generated) == mempty
          then defaultConfig
          else defaultConfig <> mempty
            { plan = mempty
              { createDbConfig = pure standardProcessConfig
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
