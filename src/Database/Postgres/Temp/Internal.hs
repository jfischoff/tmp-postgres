{-# OPTIONS_HADDOCK prune #-}
{-|
This module provides the high level functions that are re-exported
by @Database.Postgres.Temp@. Additionally it includes some
identifiers that are used for testing but are not exported.
-}
module Database.Postgres.Temp.Internal where

import Database.Postgres.Temp.Internal.Core
import Database.Postgres.Temp.Internal.Config

import           Control.Concurrent
import qualified Control.Concurrent.Async as Async
import           Control.DeepSeq
import           Control.Exception
import           Control.Monad (void, join)
import           Control.Monad.Trans.Cont
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy.Char8 as LBSC
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map.Strict as Map
import qualified Database.PostgreSQL.Simple.Options as Client
import           GHC.Generics
import           Prettyprinter
import           System.IO.Unsafe (unsafePerformIO)
import           System.Process.Typed
import           System.Directory

-- | Handle for holding temporary resources, the @postgres@ process handle
--   and @postgres@ connection information. The 'DB' also includes the
--   final plan used to start @initdb@, @createdb@ and
--   @postgres@.
--
--   @since 1.12.0.0
data DB = DB
  { dbResources :: Resources
  -- ^ Temporary resources and the final 'Plan'.
  , dbPostgresProcess :: PostgresProcess
  -- ^ @postgres@ process handle and the connection options.
  }

instance Pretty DB where
  pretty DB {..}
    =  "dbResources"
    <> softline
    <> indent 2 (pretty dbResources)
    <> hardline
    <> "dbPostgresProcess"
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
bracket (fmap 'makeDataDirectoryPermanent' 'start') (either mempty 'stop')
@


@since 1.24.0.0
-}
makeDataDirectoryPermanent :: DB -> DB
makeDataDirectoryPermanent db = db
  { dbResources = makeResourcesDataDirPermanent $ dbResources db
  }

-- | Get the directory that is used to create other temporary directories
--
--   @since 1.12.0.0
toTemporaryDirectory :: DB -> FilePath
toTemporaryDirectory = resourcesTemporaryDir . dbResources

{-|
Get the final @postgresql.conf@

@since 1.25.0.0
-}
toPostgresqlConfigFile :: DB -> String
toPostgresqlConfigFile = completePlanConfig . resourcesPlan . dbResources
-------------------------------------------------------------------------------
-- Life Cycle Management
-------------------------------------------------------------------------------
{-|
The fastest config we can make.

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

@since 1.21.0.0
-}
fastPostgresConfig :: [(String, String)]
fastPostgresConfig =
  [ ("shared_buffers", "12MB")
  , ("fsync", "off")
  , ("synchronous_commit", "off")
  , ("full_page_writes", "off")
  , ("log_min_messages", "PANIC")
  , ("log_min_error_statement", "PANIC")
  , ("log_statement", "none")
  , ("client_min_messages", "ERROR")
  , ("commit_delay", "100000")
  , ("wal_level", "minimal")
  , ("archive_mode", "off")
  , ("max_wal_senders", "0")
  ]

{-|
The default configuration. This will create a database called \"postgres\"
   via @initdb@ (it's default behavior).
   It will create a temporary directory for the data and a temporary directory
 for a unix socket and listen on 127.0.0.1 and ::1 on a random port.
 Additionally it will use the following \"postgresql.conf\"
 which is optimized for performance.

@
shared_buffers = 12MB
fsync = off
synchronous_commit = off
full_page_writes = off
log_min_messages = PANIC
log_min_error_statement = PANIC
log_statement = none
client_min_messages = ERROR
commit_delay = 100000
wal_level = minimal
archive_mode = off
max_wal_senders = 0
@

'defaultConfig' also passes the @--no-sync@ flag to @initdb@.

If you would like to customize this behavior you can start with the
'defaultConfig' and overwrite fields or combine a 'defaultConfig' with another 'Config'
 using '<>' ('mappend').

Alternatively you can eschew 'defaultConfig' altogether, however
your @postgres@ might start and run faster if you use
'defaultConfig'.

The 'defaultConfig' redirects all output to @\/dev\/null@. See
'verboseConfig' for a version that logs more output.

To append additional lines to \"postgresql.conf\" file create a
custom 'Config' like the following.

@
custom = defaultConfig <> mempty
  { 'postgresConfigFile' =
      [ ("wal_level", "replica")
      , ("archive_mode", "on")
      , ("max_wal_senders", "2")
      , ("fsync", "on")
      , ("synchronous_commit", "on")
      ]
  }
@

 As an alternative to using 'defaultConfig' one could create a
 config from connections parameters using 'optionsToDefaultConfig'.

@since 1.21.0.0
-}
defaultConfig :: Config
defaultConfig = mempty
  { postgresConfigFile = fastPostgresConfig
  , initDbConfig = pure mempty
    { commandLine = mempty
      { keyBased = Map.singleton "--no-sync" Nothing
      }
    }
  }


{-|
Default configuration for PostgreSQL versions 9.3 and greater but less
than 10.

If you get an error that \"--no-sync\" is an invalid parameter then you
should use this config.

@since 1.21.1.0
-}
defaultConfig_9_3_10 :: Config
defaultConfig_9_3_10 = mempty
  { postgresConfigFile = fastPostgresConfig
  , initDbConfig = pure mempty
    { commandLine = mempty
      { keyBased = Map.singleton "--nosync" Nothing
      }
    }
  }

-- | Default postgres options
--
--   @since 1.21.0.0
verbosePostgresConfig :: [(String, String)]
verbosePostgresConfig =
  [ ("shared_buffers", "12MB")
  , ("fsync", "off")
  , ("synchronous_commit", "off")
  , ("full_page_writes", "off")
  , ("log_min_duration_statement", "0")
  , ("client_min_messages", "WARNING")
  , ("log_min_messages", "WARNING")
  , ("log_min_error_statement", "WARNING")
  , ("log_checkpoints", "on")
  , ("log_connections", "on")
  , ("log_disconnections", "on")
  , ("log_lock_waits", "on")
  , ("log_temp_files", "0")
  , ("log_autovacuum_min_duration", "0")
  , ("log_error_verbosity", "default")
  , ("log_line_prefix", "'%t [%p]: '")
  , ("lc_messages", "'C'")
  , ("track_io_timing", "on")
  ]

{-|
This is similar to 'defaultConfig' but it logs as much as possible..

@since 1.21.0.0
-}
verboseConfig :: Config
verboseConfig = defaultConfig <> mempty
  { logger = pure print
  , postgresConfigFile = verbosePostgresConfig
  , initDbConfig = pure standardProcessConfig
  , postgresConfig = standardProcessConfig
  }

{-|
Useful options for configuring and loading @auto_explain@.

@since 1.34.1.0
-}
autoExplainPostgresConfig :: Int -> [(String, String)]
autoExplainPostgresConfig milliseconds = verbosePostgresConfig <>
  [ ("log_min_duration_statement", show milliseconds <> "ms")
  , ("shared_preload_libraries", "'auto_explain'")
  , ("session_preload_libraries", "'auto_explain'")
  , ("auto_explain.log_analyze", "1")
  , ("auto_explain.log_buffers", "1")
  , ("auto_explain.log_timing", "1")
  , ("auto_explain.log_triggers", "1")
  , ("auto_explain.log_verbose", "1")
  , ("auto_explain.log_min_duration", show milliseconds <> "ms")
  , ("auto_explain.log_nested_statements", "1")
  , ("auto_explain.sample_rate", "1")
  , ("auto_explain.log_verbose", "on")
  , ("log_connections", "off")
  , ("log_disconnections", "off")
  ]

{-|
A config which loads and configures @auto_explain@. Useful for
understanding slow queries plans.

@since 1.34.1.0
-}
autoExplainConfig
  :: Int
  -- ^ Minimum number of milliseconds to log. Use 0 to log all queries.
  -> Config
autoExplainConfig milliseconds = defaultConfig <> mempty
  { postgresConfigFile = autoExplainPostgresConfig milliseconds
  , postgresConfig = standardProcessConfig
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

Additionally the @generated@ `Config` also:

* Sets a `connectionTimeout` of one minute.
* Redirects output to @\/dev\/null@.

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
--   @since 1.21.0.0
start :: IO (Either StartError DB)
start = startConfig defaultConfig

-- | Stop the @postgres@ process and cleanup any temporary resources that
--   might have been created.
--
--   @since 1.12.0.0
stop :: DB -> IO ()
stop DB {..} =
  Async.concurrently_ (stopPlan dbPostgresProcess) $ cleanupConfig dbResources

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

stopPostgresGracefullyOrThrow :: DB -> IO ()
stopPostgresGracefullyOrThrow db = do
    let postgresProcess = dbPostgresProcess db
    exitCode <- stopPostgresProcess True postgresProcess
    case exitCode of
        ExitSuccess -> pure ()
        ExitFailure _ ->
            throwIO $ ExitCodeException
              { eceExitCode = exitCode
              , eceProcessConfig = postgresProcessConfig postgresProcess
              , eceStdout = LBS.empty
              , eceStderr = LBS.empty
              }

-- | Restart the @postgres@ from 'DB' using the prior 'Config'. This
--   will also start an instance previously stoppped with 'stopPostgres'.
--
--   @since 1.12.0.0
restart :: DB -> IO (Either StartError DB)
restart db@DB{..} = try $ do
  void $ stopPostgres db
  let Plan{..} = resourcesPlan dbResources
      startAction = startPostgresProcess completePlanConnectionTimeout completePlanLogger
        completePlanPostgres
  bracketOnError startAction stopPlan $ \result ->
    pure $ db { dbPostgresProcess = result }
-------------------------------------------------------------------------------
-- Exception safe interface
-------------------------------------------------------------------------------
{-|
Exception safe database create with options. See 'startConfig' for more
details. Calls 'stop' even in the face of exceptions.

@since 1.21.0.0
-}
withConfig :: Config
         -- ^ The @extra@ 'Config' combined with the @generated@ 'Config'. See
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

@since 1.21.0.0
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
--   want to create a database
--
--   * owned by a specific user you will also login with
--   * with a specific name (i.e. not the default name, "postgres")
--
--  among other use cases. Changing the 'connectionOptions' field of
--  'Config' does /not/ achieve these results and you are likely to see
--  unexpected behaviour if you try to.
--
--   @since 1.21.0.0
optionsToDefaultConfig :: Client.Options -> Config
optionsToDefaultConfig opts = defaultConfig <> optionsToConfig opts

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

@since 1.25.0.0
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

@since 1.25.0.0
-}
data Cache = Cache
  { cacheResourcesCow :: Bool
  , cacheResourcesDirectory :: CompleteDirectoryType
  } deriving stock (Generic)
    deriving anyclass (NFData)

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
  (_exitCode, stderr) <- readProcessStderr $ proc "cp" [cpFlag]
  -- if the flags do not exist we get a message like "cp: illegal option"
  let usage = "usage:" -- macos
      missingFile = "cp: missing file operand" -- linux
      stderrString = LBSC.unpack stderr
  pure $ usage == take (length usage) stderrString
       || missingFile == take (length missingFile) stderrString
{-# NOINLINE cowCheck #-}

cpFlags :: [String]
cpFlags =
  ["-R"]
  ++ if cowCheck
#ifdef darwin_HOST_OS
  then ["-c"]
#else
  then ["--reflink=auto"]
#endif
  else []

{-|
'defaultCacheConfig' attempts to determine if the @cp@ on the path
supports \"copy on write\" flags and if it does, sets 'cacheUseCopyOnWrite'
to 'True'.

It sets 'cacheDirectoryType' to 'Permanent' @~\/.tmp-postgres@ and
'cacheTemporaryDirectory' to @\/tmp@ (but this is not used when
'Permanent' is set).

@since 1.25.0.0
-}
defaultCacheConfig :: CacheConfig
defaultCacheConfig = CacheConfig
  { cacheDirectoryType = Permanent "~/.tmp-postgres"
  , cacheTemporaryDirectory = "/tmp"
  , cacheUseCopyOnWrite = cowCheck
  }

{-|
Setup the @initdb@ cache folder.

@since 1.25.0.0
-}
setupInitDbCache
  :: CacheConfig
  -> IO Cache
setupInitDbCache CacheConfig {..} =
  bracketOnError
    (setupDirectoryType
      cacheTemporaryDirectory
      "tmp-postgres-cache"
      cacheDirectoryType
    )
    cleanupDirectoryType $ pure . Cache cacheUseCopyOnWrite

{-|
Cleanup the cache directory if it was 'Temporary'.

@since 1.25.0.0
-}
cleanupInitDbCache :: Cache -> IO ()
cleanupInitDbCache = cleanupDirectoryType . cacheResourcesDirectory

{-|
Enable @initdb@ data directory caching. This can lead to a 4x speedup.

Exception safe version of 'setupInitDbCache'. Equivalent to

@
'withDbCacheConfig' = bracket ('setupInitDbCache' config) 'cleanupInitDbCache'
@

@since 1.25.0.0
-}
withDbCacheConfig
  :: CacheConfig
  -- ^ Configuration
  -> (Cache -> IO a)
  -- ^ action for which caching is enabled
  -> IO a
withDbCacheConfig config =
  bracket (setupInitDbCache config) cleanupInitDbCache

{-|
Equivalent to 'withDbCacheConfig' with the 'CacheConfig'
'defaultCacheConfig' makes.

Here is an example using caching:

@
withDbCache $ \\cache -> do
  withConfig (cacheConfig cache) $ \\db -> ...
  withConfig (cacheConfig cache) $ \\db -> ...
@

@since 1.25.0.0
-}
withDbCache :: (Cache -> IO a) -> IO a
withDbCache = withDbCacheConfig defaultCacheConfig

{-|
Helper to make a 'Config' out of caching info.

@since 1.25.0.0
-}
cacheConfig :: Cache -> Config
cacheConfig Cache {..} = mempty
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
  deriving stock (Generic)
  deriving anyclass (NFData)

{- |
Shutdown the database and copy the directory to a folder.

@since 1.29.0.0
-}
takeSnapshot
  :: DB
  -- ^ The handle. The @postgres@ is shutdown and the data directory is copied.
  -> IO (Either StartError Snapshot)
takeSnapshot db = try $ do
  stopPostgresGracefullyOrThrow db
  bracketOnError
    (setupDirectoryType
      (toTemporaryDirectory db)
      "tmp-postgres-snapshot"
      Temporary
    )
    cleanupDirectoryType $ \snapShotDir -> do
      let snapshotCopyFlags = cpFlags ++ [toDataDirectory db, toFilePath snapShotDir]
          snapshotCopyCmd = proc "cp" snapshotCopyFlags

      (readProcess_ snapshotCopyCmd >> (pure $ Snapshot snapShotDir))
        `catch` (throwIO . SnapshotCopyFailed)

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

Here is an example with caching and snapshots:

@
withDbCache $ \\cache -> withConfig (cacheConfig cache) $ \\db ->
  migrate db
  withSnapshot Temporary db $ \\snapshot -> do
    withConfig (snapshotConfig db) $ \\migratedDb -> ...
    withConfig (snapshotConfig db) $ \\migratedDb -> ...
    withConfig (snapshotConfig db) $ \\migratedDb -> ...
@

The 'Snapshot's are ephemeral. If you would like the 'Snapshot's to persistent
consider using 'cacheAction' instead.

@since 1.29.0.0
-}
withSnapshot
  :: DB
  -> (Snapshot -> IO a)
  -> IO (Either StartError a)
withSnapshot db f = bracket
  (takeSnapshot db)
  (either mempty cleanupSnapshot)
  (either (pure . Left) (fmap Right . f))

-- Helper for 'snapshotConfig' and 'cacheAction'
fromFilePathConfig :: FilePath -> Config
fromFilePathConfig filePath = mempty
  { copyConfig = pure $ pure CopyDirectoryCommand
      { sourceDirectory = filePath
      , destinationDirectory = Nothing
      , useCopyOnWrite = cowCheck
      }
  , initDbConfig = Zlich
  }

{-|
Convert a snapshot into a 'Config' that includes a 'copyConfig' for copying the
snapshot directory to a temporary directory.

@since 1.20.0.0
-}
snapshotConfig :: Snapshot -> Config
snapshotConfig = fromFilePathConfig . toFilePath . unSnapshot

-------------------------------------------------------------------------------
-- cacheAction
-------------------------------------------------------------------------------
cacheActionLocks :: MVar (Map.Map FilePath (MVar ()))
cacheActionLocks = unsafePerformIO $ newMVar mempty
{-# NOINLINE cacheActionLocks #-}

withActionLock :: FilePath -> IO a -> IO a
withActionLock filePath action = do
  theLock <- modifyMVar cacheActionLocks $ \theMap -> do
    theLock <- case Map.lookup filePath theMap of
      Nothing -> newMVar ()
      Just x  -> pure x
    pure (Map.insert filePath theLock theMap, theLock)

  withMVar theLock $ \_ -> action

{-|
Check to see if a cached data directory exists.

If the file path does not exist the @initial@ config is used to start a @postgres@
instance. After which the @action@ is applied, the data directory is cached
and @postgres@ is shutdown.

'cacheAction' 'mappend's a config to copy the cached data directory
on startup onto the @initial@ config and returns it. In other words:

@
initialConfig <> configFromCachePath
@

'cacheAction' can be used to create a snapshot of migrated database and not
remigrate as long as the migration does not change. See 'withSnapshot' for
a ephemeral version of taking snapshots.

You can nest calls to cacheAction and safe to call it from several threads.
However 'cacheAction' uses locks internal to prevent multiple threads from
stomping on each other.

If one makes a nested call and accidently uses the same cache directory
in both calls the calls will deadlock. If this occurs on the same thread
RTS will throw an exception. However do not rely on this and just be
careful to not reuse the same cache path when nesting calls.

There is no good reuse the cache path when nesting so one is unlikely to
run into this.

@since 1.34.0.0
-}
cacheAction
  :: FilePath
  -- ^ Location of the data directory cache.
  -> (DB -> IO ())
  -- ^ @action@ to cache.
  -> Config
  -- ^ @initial@ 'Config'.
  -> IO (Either StartError Config)
cacheAction cachePath action config = do
  fixCachePath <- fixPath cachePath
  let result = config <> fromFilePathConfig fixCachePath

  withActionLock fixCachePath $ do
    nonEmpty <- doesFileExist $ fixCachePath <> "/PG_VERSION"

    if nonEmpty then pure $ pure result else fmap join $ withConfig config $ \db -> do
      action db
      -- TODO see if parallel is better
      stopPostgresGracefullyOrThrow db
      createDirectoryIfMissing True fixCachePath

      let snapshotCopyFlags = cpFlags ++ [toDataDirectory db, fixCachePath]
          snapshotCopyCmd = proc "cp" snapshotCopyFlags

      (readProcess_ snapshotCopyCmd >> (pure $ Right result))
        `catch` (pure . Left . SnapshotCopyFailed)
