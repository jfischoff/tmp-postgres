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
import           System.IO.Unsafe (unsafePerformIO)
import           System.Process
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
-- initdb cache
-------------------------------------------------------------------------------
{-|
Configuration for the @initdb@ data directory cache.

@since 1.20.0.0
-}
data CacheConfig = CacheConfig
  { cacheTemporaryDirectory :: FilePath
  -- ^ Root temporary directory used if 'cacheDirectoryType' is set to
  -- 'Temporary'. @\/tmp@ is a good default.
  , cacheDirectoryType      :: DirectoryType
  -- ^ Used to specify is a 'Permanent' or 'Temporary' directory should be
  --   used. 'defaultCacheConfig' uses 'Permanent' @~\/.tmp-postgres@
  --   by default.
  , cacheUseCopyOnWrite     :: Bool
  -- ^ Some operatoring system versions support flags for @cp@ that allow
  --   \"copy on write\" which is about 2x faster. 'defaultCacheConfig'
  --   attempts to determine if the @cp@ on the path supports copy on write
  --   and sets this to 'True' if it does.
  }

{-|
A handle to cache temporary resources and configuration.

@since 1.20.0.0
-}
data CacheResources = CacheResources
  { cacheResourcesCow :: Bool
  , cacheResourcesDirectory :: CompleteDirectoryType
  }

-- | A bool that is 'True' if the @cp@ on the path supports \"copy on write\"
--   flags.
cowCheck :: Bool
cowCheck = unsafePerformIO $ do
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
  pure $ usage ==  take (length usage) errorOutput
       || missingFile ==  take (length missingFile) errorOutput
{-# NOINLINE cowCheck #-}

{-|
'defaultCacheConfig' attempts to determine if the @cp@ on the path
supports \"copy on write\" flags and if it does, sets 'cacheUseCopyOnWrite'
to 'True'.

It sets 'cacheDirectoryType' to 'Permanent' @~\/.tmp-postgres@ and
'cacheTemporaryDirectory' to @\/tmp@ (but this is not used when
'Permanent' is set).

@since 1.19.0.0
-}
defaultCacheConfig :: CacheConfig
defaultCacheConfig = CacheConfig
  { cacheDirectoryType = Permanent "~/.tmp-postgres"
  , cacheTemporaryDirectory = "/tmp"
  , cacheUseCopyOnWrite = cowCheck
  }

-- | Setup the @initdb@ cache folder.
setupInitDbCache
  :: CacheConfig
  -> IO CacheResources
setupInitDbCache CacheConfig {..} =
  bracketOnError
    (setupDirectoryType
      cacheTemporaryDirectory
      "tmp-postgres-cache"
      cacheDirectoryType
    )
    cleanupDirectoryType $ pure . CacheResources cacheUseCopyOnWrite

{-|
Cleanup the cache directory if it was 'Temporary'.

@since 1.20.0.0
-}
cleanupInitDbCache :: CacheResources -> IO ()
cleanupInitDbCache = cleanupDirectoryType . cacheResourcesDirectory

{-|
Enable @initdb@ data directory caching. This can lead to a 4x speedup.

Exception safe version of 'setupInitDbCache'. Equivalent to

@
   'withDbCacheConfig' = bracket ('setupInitDbCache' config) 'cleanupInitDbCache'
@

@since 1.20.0.0
-}
withDbCacheConfig
  :: CacheConfig
  -- ^ Configuration
  -> (CacheResources -> IO a)
  -- ^ action for which caching is enabled
  -> IO a
withDbCacheConfig config =
  bracket (setupInitDbCache config) cleanupInitDbCache

{-|
Equivalent to 'withDbCacheConfig' with the 'CacheConfig'
'defaultCacheConfig' makes.

@since 1.20.0.0
-}
withDbCache :: (CacheResources -> IO a) -> IO a
withDbCache = withDbCacheConfig defaultCacheConfig

{-|
Helper to make a 'Config' out of caching info.

@since 1.20.0.0
-}
toCacheConfig :: CacheResources -> Config
toCacheConfig CacheResources {..} = mempty
  { initDbCache = pure $ pure
      (cacheResourcesCow, toFilePath cacheResourcesDirectory)
  }

-------------------------------------------------------------------------------
-- withSnapshot
-------------------------------------------------------------------------------
{-|
A type to track a possibly temporary snapshot directory

@since 1.20.0.0
-}
newtype Snapshot = Snapshot { unSnapshot :: CompleteDirectoryType }

{- |
Shutdown the database and copy the directory to a folder.

@since 1.20.0.0
-}
takeSnapshot
  :: DirectoryType
  -- ^ Either a 'Temporary' or preexisting 'Permanent' directory.
  -> DB
  -- ^ The handle. The @postgres@ is shutdown and the data directory is copied.
  -> IO (Either StartError Snapshot)
takeSnapshot directoryType db = try $ do
  throwIfNotSuccess id =<< stopPostgresGracefully db
  let
#ifdef darwin_HOST_OS
    cpFlags = if cowCheck then "cp -Rc " else "cp -R "
#else
    cpFlags = if cowCheck then "cp -R --reflink=auto " else "cp -R "
#endif
  bracketOnError
    (setupDirectoryType
      (toTemporaryDirectory db)
      "tmp-postgres-snapshot"
      directoryType
    )
    cleanupDirectoryType $ \snapShotDir -> do
      let snapshotCopyCmd = cpFlags <>
            toDataDirectory db <> "/* " <> toFilePath snapShotDir
      throwIfNotSuccess (SnapshotCopyFailed snapshotCopyCmd) =<<
        system snapshotCopyCmd

      pure $ Snapshot snapShotDir

{-|
Cleanup any temporary resources used for the snapshot.

@since 1.20.0.0
-}
cleanupSnapshot :: Snapshot -> IO ()
cleanupSnapshot = cleanupDirectoryType . unSnapshot

{- |
Exception safe method for taking a file system level copy of the database cluster.

Snapshots are useful if you would like to start every test from a migrated database
and the migration process is more time consuming then copying the additional data.

@since 1.20.0.0
-}
withSnapshot
  :: DirectoryType
  -> DB
  -> (Snapshot -> IO a)
  -> IO (Either StartError a)
withSnapshot dirType db f = bracket
  (takeSnapshot dirType db)
  (either mempty cleanupSnapshot)
  (either (pure . Left) (fmap Right . f))

{-|
Convert a snapshot into a 'Config' that includes a 'copyConfig' for copying the
snapshot directory to a temporary directory.

@since 1.20.0.0
-}
snapshotConfig :: Snapshot -> Config
snapshotConfig (Snapshot savePointPath) = mempty
  { plan = mempty
      { copyConfig = pure $ pure CopyDirectoryCommand
          { sourceDirectory = toFilePath savePointPath
          , destinationDirectory = Nothing
          , useCopyOnWrite = cowCheck
          }
      , initDbConfig = Zlich
      }
  }
