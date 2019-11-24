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
import           Data.Maybe
import           Data.Monoid
import           Data.String
import qualified Database.PostgreSQL.Simple as PG
import qualified Database.PostgreSQL.Simple.Options as Client
import           System.Environment
import           System.Exit (ExitCode(..))
import           System.Process
import           System.Random
import           Text.PrettyPrint.ANSI.Leijen hiding ((<$>))

-- | Handle for holding temporary resources, the @postgres@ process handle
--   and postgres connection information. The 'DB' also includes the
--   final plan used to start @initdb@, @createdb@ and
--   @postgres@. See 'toConnectionString' or 'toConnectionOptions'
--   for converting a 'DB' to postgresql connection string.
--
--   @since 1.12.0.0
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
--   'Client.Options' using 'toConnectionOptions'.
--
--   @since 1.12.0.0
toConnectionString :: DB -> ByteString
toConnectionString
  = Client.toConnectionString
  . toConnectionOptions

-- | Convert a 'DB' to a connection 'Client.Options' type.
--
--   @since 1.12.0.0
toConnectionOptions :: DB -> Client.Options
toConnectionOptions
  = postgresProcessClientOptions
  . dbPostgresProcess

-- | Access the data directory. This was either generated or
--   specified explicitly when creating the 'Config'
--
--   @since 1.12.0.0
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


@since 1.12.0.0
-}
makeDataDirPermanent :: DB -> DB
makeDataDirPermanent db = db
  { dbResources = makeResourcesDataDirPermanent $ dbResources db
  }

-- | Get the directory that is used to create other temporary directories
--
--   @since 1.12.0.0
toTemporaryDirectory :: DB -> FilePath
toTemporaryDirectory = resourcesTemporaryDir . dbResources
-------------------------------------------------------------------------------
-- Life Cycle Management
-------------------------------------------------------------------------------
-- | Default postgres options
--
--   @since 1.12.0.0
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

The 'defaultConfig' also disables the logging of internal 'Event's.

To append additional lines to \"postgresql.conf\" file create a
custom 'Config' like the following.

 @
  custom = defaultConfig <> mempty
    { 'plan' = mempty
      { 'postgresConfigFile' =
          [ "wal_level = replica"
          , "archive_mode = on"
          , "max_wal_senders = 2"
          , "fsync = on"
          , "synchronous_commit = on"
          ]
      }
    }
 @

Or using the provided lenses and your favorite lens library:

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
 config from connections parameters using 'optionsToDefaultConfig'.


@since 1.12.0.0
-}
defaultConfig :: Config
defaultConfig = mempty
  { plan = mempty
    { logger = pure mempty
    , postgresConfigFile = defaultPostgresConfig
    , createDbConfig = Nothing
    , initDbConfig = pure mempty
      { commandLine = mempty
        { keyBased = Map.singleton "--no-sync" Nothing
        }
      }
    }
  }

{-|
'mappend' the 'defaultConfig' with a 'Config' that provides additional
   \"postgresql.conf\" lines. Equivalent to:

@
'defaultPostgresConf' extra = 'defaultConfig' <> mempty
  { 'plan' = mempty
    { 'postgresConfigFile' = extra
    }
  }
@

or with lenses:

@
'defaultPostgresConf' extra = 'defaultConfig' & 'planL' . 'postgresConfigFile' <>~ extra
@

@since 1.12.0.0
-}
defaultPostgresConf :: [String] -> Config
defaultPostgresConf extra = defaultConfig <> mempty
  { plan = mempty
    { postgresConfigFile = extra
    }
  }


{-|
A config that logs as little as possible.

@since 1.14.0.0
-}
silentPostgresConfig :: [String]
silentPostgresConfig =
  [ "shared_buffers = 12MB"
  , "fsync = off"
  , "synchronous_commit = off"
  , "full_page_writes = off"
  , "log_min_messages = PANIC"
  , "log_min_error_statement = PANIC"
  , "log_statement = none"
  , "client_min_messages = ERROR"
  ]

{-|
The similar to 'defaultConfig' but all the handles are set to @\/dev\/null@.
and uses a @postgresql.conf@ which disables logging:

 @
   shared_buffers = 12MB
   fsync = off
   synchronous_commit = off
   full_page_writes = off
   log_min_messages = PANIC
   log_min_error_statement = PANIC
   log_statement = none
   client_min_messages = ERROR
 @

See 'silentProcessConfig' as well.

@since 1.14.0.0
-}
silentConfig :: Config
silentConfig = defaultConfig <> mempty
  { plan = mempty
    { postgresConfigFile = silentPostgresConfig
    , initDbConfig = pure silentProcessConfig
    , postgresPlan = mempty
        { postgresConfig = silentProcessConfig
        }
    }
  }

{-|

Create zero or more temporary resources and use them to make a 'Config'.

The passed in config is inspected and a generated config is created.
The final config is built by

 @
   generated '<>' extra
 @

Based on the value of 'socketDirectory' a \"postgresql.conf\" is created with:

 @
   listen_addresses = '127.0.0.1, ::1'
   unix_socket_directories = \'SOCKET_DIRECTORY\'
 @

Additionally the @generated@ `Config` also does the following:

* Sets a `connectionTimeout` of one minute.
* Logs internal `Event`s.
* Sets the processes to use the standard input and output handles.
* Sets the 'dataDirectoryString' to file path generated from 'dataDirectory'.

All of these values can be overrided by the @extra@ config.

The returned 'DB' requires cleanup. `startConfig` should be
used with a `bracket` and 'stop', e.g.

 @
   `withConfig` :: `Config` -> (`DB` -> IO a) -> IO (Either `StartError` a)
   'withConfig' plan f = `bracket` (`startConfig` plan) (either mempty `stop`) $
      either (pure . Left) (fmap Right . f)
 @

or just use 'withConfig'. If you are calling 'startConfig' you
probably want 'withConfig' anyway.

@since 1.15.0.0
-}
startConfig :: Config
          -- ^ @extra@ configuration that is 'mappend'ed last to the generated `Config`.
          -- @generated@ '<>' @extra@.
          -> IO (Either StartError DB)
startConfig extra = try $ evalContT $ do
  dbResources@Resources {..} <-
    ContT $ bracketOnError (setupConfig extra) cleanupConfig
  dbPostgresProcess <-
    ContT $ bracketOnError (startPlan resourcesPlan) stopPlan
  pure DB {..}

-- | Default start behavior. Equivalent to calling 'startConfig' with the
--   'defaultConfig'.
--
--   @since 1.12.0.0
start :: IO (Either StartError DB)
start = startConfig defaultConfig

-- | Stop the @postgres@ process and cleanup any temporary resources that
--   might have been created.
--
--   @since 1.12.0.0
stop :: DB -> IO ()
stop DB {..} = do
  void $ stopPlan dbPostgresProcess
  cleanupConfig dbResources

-- | Only stop the @postgres@ process but leave any temporary resources.
--   Useful for testing backup strategies when used in conjunction with
--   'restart' or 'withRestart'.
--
--   @since 1.12.0.0
stopPostgres :: DB -> IO ExitCode
stopPostgres = stopPlan . dbPostgresProcess

-- | Only stop the @postgres@ process but leave any temporary resources.
--   In contrast to 'stopPostgres' this function makes sure @postgres@
--   has time to properly write files to the data directory.
--
--   @since 1.16.1.0
stopPostgresGracefully :: DB -> IO ExitCode
stopPostgresGracefully = stopPostgresProcess True . dbPostgresProcess

-- | Restart the @postgres@ from 'DB' using the prior 'Plan'.
--
--   @since 1.12.0.0
restart :: DB -> IO (Either StartError DB)
restart db@DB{..} = try $ do
  void $ stopPostgres db
  let CompletePlan{..} = resourcesPlan dbResources
      startAction = startPostgresProcess completePlanConnectionTimeout completePlanLogger
        completePlanPostgres
  bracketOnError startAction stopPlan $ \result ->
    pure $ db { dbPostgresProcess = result }

-- | Reload the configuration file without shutting down. Calls
--   @pg_reload_conf()@.
--
--   @since 1.12.0.0
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

@since 1.15.0.0
-}
withConfig :: Config
         -- ^ @extra@. 'Config' combined with the generated 'Config'. See
         -- 'startConfig' for more info.
         -> (DB -> IO a)
         -- ^ @action@ continuation.
         -> IO (Either StartError a)
withConfig extra f = bracket (startConfig extra) (either mempty stop) $
  either (pure . Left) (fmap Right . f)

{-| Default expectation safe interface. Equivalent to

 @
   'with' = 'withConfig' 'defaultConfig'
 @

@since 1.15.0.0
-}
with :: (DB -> IO a)
     -- ^ @action@ continuation.
     -> IO (Either StartError a)
with = withConfig defaultConfig

-- | Exception safe version of 'restart'.
--
--   @since 1.12.0.0
withRestart :: DB -> (DB -> IO a) -> IO (Either StartError a)
withRestart db f = bracket (restart db) (either mempty stop) $
  either (pure . Left) (fmap Right . f)

-- | Attempt to create a 'Config' from a 'Client.Options'. Useful if you
--   want to create a database owned by a specific user you will also login
--   with among other use cases.
--
--   @since 1.15.0.0
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
-- | Display a 'DB'.
--
--   @since 1.12.0.0
prettyPrintDB :: DB -> String
prettyPrintDB = show . pretty

-------------------------------------------------------------------------------
-- withNewDb
-------------------------------------------------------------------------------
-- | Drop the db if it exists. Terminates all connections to the db first.
--
--   @since 1.12.0.0
dropDbIfExists :: Client.Options -> String -> IO ()
dropDbIfExists options dbName = do
  let theConnectionString = Client.toConnectionString options
      dropDbQuery = fromString $ "DROP DATABASE IF EXISTS " <> dbName <> ";"

  terminateConnections $ options
    { Client.dbname = pure dbName
    }

  mapException DeleteDbError $
    bracket (PG.connectPostgreSQL theConnectionString) PG.close $
      \conn -> void $ PG.execute_ conn dropDbQuery

{-|
Use the current database as a template and make a copy. Give the
copy a random name.

Equivalent to:

@
 'withNewDb' = 'withNewDbConfig' mempty
@

See 'startNewDbConfig' for more details.

@since 1.12.0.0
-}
withNewDb
  :: DB
  -- ^ The original 'DB' handle. The database name specified in the
  --   connection options
  --   is used as the template for the @generated@ 'ProcessConfig'
  -> (DB -> IO a)
  -- ^ The modified 'DB' handle that has the new database name
  --   in it's connection options.
  -> IO (Either StartError a)
withNewDb = withNewDbConfig mempty

{-|
Exception safe version of 'startNewDbConfig'. Creates a
temporary database using the current database as a template.

See 'startNewDbConfig' for more details.

@since 1.12.0.0
-}
withNewDbConfig
  :: ProcessConfig
  -- ^ @extra@ @createdb@ 'ProcessConfig'.
  -> DB
  -- ^ The original 'DB' handle. The database name specified in the
  --   connection options
  --   is used as the template for the @generated@ 'ProcessConfig'.
  -> (DB -> IO a)
  -- ^ The modified 'DB' handle that has the new database name
  --   in it's connection options.
  -> IO (Either StartError a)
withNewDbConfig extra db f =
  bracket (startNewDbConfig extra db) (either mempty stopNewDb) $
    either (pure . Left) (fmap Right . f)

{-|
Use the current database as a template and make a copy. Give the
copy a random name.

Equivalent to:

@
 'startNewDb' = 'startNewDbConfig' mempty
@

See 'startNewDbConfig' for more details.

@since 1.13.0.0
-}
startNewDb
  :: DB
  -- ^ The original 'DB' handle. The database name specified in the
  --   connection options
  --   is used as the template for the @generated@ 'ProcessConfig'.
  -> IO (Either StartError DB)
startNewDb = startNewDbConfig mempty

{-|
Use the current database as a template and make a copy. Give the
copy a random name.

Copying a database from a template can be faster than creating a new
@postgres@ and migrating a database from scratch. In artifical benchmarks
it appears to be about 2x faster.

To use the current database as a template all connections to the database
must be terminated first.

To override the arguments passed to @createdb@ one can pass in @extra@
'ProcessConfig'. The @combined@ process is created by 'mappend'ed the
@generated@ with the @extra@ 'ProcessConfig', e.g.

@
   combined = generated '<>' extra
@

The current implementation has a few known issues.

If a connection is made between the termination command and the @createdb@
call the @createdb@ call will fail.

Additionally the generated name is 32 character random name of characters
\"a\" to \"z\". It is possible, although unlikeily that a duplicate
database name could be generated and this would also cause a failure.

'startNewDbConfig' requires cleanup so it best to use a 'bracket' along
with 'stopNewDb'. This is equivalient to 'withNewDbConfig'.

The only reason to use 'startNewDbConfig' over 'withNewDbConfig' is
if you are unable to use 'withNewDbConfig' for some reason.

@since 1.13.0.0
-}
startNewDbConfig
  :: ProcessConfig
  -- ^ @extra@ @createdb@ 'ProcessConfig'.
  -> DB
  -- ^ The original 'DB' handle. The database name specified in the
  --   connection options
  --   is used as the template for the @generated@ 'ProcessConfig'.
  -> IO (Either StartError DB)
startNewDbConfig extra@ProcessConfig{..} db = try $ do
  stdGen <- getStdGen
  let oldOptions@Client.Options {..} = toConnectionOptions db
      theDbName = fromMaybe "postgres" $ getLast dbname
      newDbName = fromMaybe (take 32 $ randomRs ('a', 'z') stdGen) $
        Map.lookup 0 $ indexBased commandLine
      newOptions = oldOptions
        { Client.dbname = pure newDbName
        }
      generated = standardProcessConfig
        { commandLine = mempty
          { keyBased = Map.fromList
              [ ("-T", Just theDbName)
              , ("-h", Just $ fromMaybe "127.0.0.1" $ getLast host)
              , ("-p ", Just $ maybe "5432" show $ getLast port)
              ]
          , indexBased = Map.singleton 0 newDbName
          }
        }
      combined = generated <> extra
      newDb = db
        { dbPostgresProcess = (dbPostgresProcess db)
            { postgresProcessClientOptions = newOptions
            }
        }
  envs <- getEnvironment
  final <- case completeProcessConfig envs combined of
    -- Failure case does not look possible
    Left errs -> throwIO $ CompleteProcessConfigFailed (show $ pretty combined) errs
    Right x -> pure x
  terminateConnections oldOptions
  executeCreateDb final
  pure newDb

-- | Cleanup the temporary database created by 'startNewDbConfig'
--   or 'startNewDb'.
--
--   @since 1.13.0.0
stopNewDb :: DB -> IO ()
stopNewDb db = do
  let oldOptions@Client.Options{..} = toConnectionOptions db
  -- This should not happen.
  newDbName <- maybe
    (throwIO $ userError "stopNewDb: missing db name!")
    pure $
    getLast dbname
  let template1Options = oldOptions
        { Client.dbname = pure "template1"
        }
  dropDbIfExists template1Options newDbName

-------------------------------------------------------------------------------
-- initdb cache
-------------------------------------------------------------------------------
{-|
Configuration for the @initdb@ data directory cache.

@since 1.15.1.0
-}
data CacheConfig = CacheConfig
  { cacheTemporaryDirectory :: FilePath
  -- ^ Root temporary directory used if 'cacheDirectoryType' is set to
  -- 'Temporary'. @\/tmp@ is a good default.
  , cacheDirectoryType      :: DirectoryType
  -- ^ Used to specify is a 'Permanent' or 'Temporary' directory should be
  --   used. 'createDefaultCacheConfig' uses 'Permanent' @~\/.tmp-postgres@
  --   by default.
  , cacheUseCopyOnWrite     :: Bool
  -- ^ Some operatoring system versions support flags for @cp@ that allow
  --   \"copy on write\" which is about 2x faster. 'createDefaultCacheConfig'
  --   attempts to determine if the @cp@ on the path supports copy on write
  --   and sets this to 'True' if it does.
  }

{-|
'createDefaultCacheConfig' attempts to determine if the @cp@ on the path
supports \"copy on write\" flags and if it does, sets 'cacheUseCopyOnWrite'
to 'True'.

It sets 'cacheDirectoryType' to 'Permanent' @~\/.tmp-postgres@ and
'cacheTemporaryDirectory' to @\/tmp@ (but this is not used when
'Permanent' is set).

@since 1.15.1.0
-}
createDefaultCacheConfig :: IO CacheConfig
createDefaultCacheConfig = do
  let
#ifdef darwin_HOST_OS
    cpFlag = "-c"
#else
    cpFlag = "--reflink=auto"
#endif
  (_, _, errorOutput)<- readProcessWithExitCode "cp" [cpFlag] ""
  -- if the flags do not exist we get a message like "cp: illegal option"
  let usage = "usage:" -- macos
      missingFile = "cp: missing file operand" -- linux
      cacheUseCopyOnWrite = usage ==  take (length usage) errorOutput
        || missingFile ==  take (length missingFile) errorOutput
      cacheDirectoryType = Permanent "~/.tmp-postgres"
      cacheTemporaryDirectory = "/tmp"
  pure CacheConfig {..}

-- | Setup the @initdb@ cache folder.
setupInitDbCache
  :: CacheConfig
  -> IO (Bool, CompleteDirectoryType)
setupInitDbCache CacheConfig {..} =
  bracketOnError
    (setupDirectoryType
      cacheTemporaryDirectory
      "tmp-postgres-cache"
      cacheDirectoryType
    )
    cleanupDirectoryType $ pure . (cacheUseCopyOnWrite,)

{-|
Cleanup the cache directory if it was 'Temporary'.

@since 1.15.1.0
-}
cleanupInitDbCache :: (Bool, CompleteDirectoryType) -> IO ()
cleanupInitDbCache = cleanupDirectoryType . snd

{-|
Enable @initdb@ data directory caching. This can lead to a 4x speedup.

Exception safe version of 'setupInitDbCache'. Equivalent to

@
   'withDbCacheConfig' = bracket ('setupInitDbCache' config) 'cleanupInitDbCache'
@

@since 1.15.1.0
-}
withDbCacheConfig
  :: CacheConfig
  -- ^ Configuration
  -> ((Bool, CompleteDirectoryType) -> IO a)
  -- ^ action for which caching is enabled
  -> IO a
withDbCacheConfig config =
  bracket (setupInitDbCache config) cleanupInitDbCache

{-|
Equivalent to 'withDbCacheConfig' with the 'CacheConfig'
'createDefaultCacheConfig' makes.

@since 1.15.1.0
-}
withDbCache :: ((Bool, CompleteDirectoryType) -> IO a) -> IO a
withDbCache action =
  flip withDbCacheConfig action =<< createDefaultCacheConfig

{-|
Helper to make a 'Config' out of caching info.

@since 1.15.1.0
-}
toCacheConfig :: (Bool, CompleteDirectoryType) -> Config
toCacheConfig cacheInfo = mempty
  { initDbCache = pure $ pure $ fmap toFilePath cacheInfo
  }

-------------------------------------------------------------------------------
-- withSnapshot
-------------------------------------------------------------------------------
type Snapshot = FilePath

-- I need to be able to make a cache
-- looks
-- snapshot are the general case
-- everything has a template directory
-- if you are making one this is unnecessarily slow
-- a pretty rare case
-- but it seems like layer on top
-- it is really a copy step
withSnapshot :: DirectoryType -> DB -> ((CompleteDirectoryType, Snapshot) -> IO a) -> a
withSnapshot = undefined

withFromSnapshot :: DB -> Snapshot -> (DB -> IO a) -> IO a
withFromSnapshot = undefined
