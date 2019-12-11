module Main where
import Control.DeepSeq
import Control.Exception
import Control.Monad
import Criterion.Main hiding (defaultConfig)
import Data.String
import Database.Postgres.Temp.Internal
import Database.Postgres.Temp.Internal.Core
import Database.Postgres.Temp.Internal.Config
import qualified Database.PostgreSQL.Simple as PG
import           System.IO.Temp (createTempDirectory, withTempDirectory)
import qualified Database.PostgreSQL.Simple.Options as Options

data Once a = Once { unOnce :: a }

instance NFData (Once a) where
  rnf x = seq x ()

defaultConfigDefaultInitDb :: Config
defaultConfigDefaultInitDb = mempty
  { logger = pure mempty
  , postgresConfigFile = fastPostgresConfig
  , initDbConfig = pure mempty
  }

createFooDb :: PG.Connection -> Int -> IO ()
createFooDb conn index = void $ PG.execute_ conn $ fromString $ unlines
  [ "CREATE TABLE foo" <> show index
  , "( id int"
  , ");"
  ]

migrateDb :: DB -> IO ()
migrateDb db = do
  let theConnectionString = toConnectionString db

  bracket (PG.connectPostgreSQL theConnectionString) PG.close $
      \conn -> mapM_ (createFooDb conn) [0 .. 100]

testQuery :: DB -> IO ()
testQuery db = do
  let theConnectionString = toConnectionString db

  bracket (PG.connectPostgreSQL theConnectionString) PG.close $
    \conn -> void $ PG.execute_ conn "INSERT INTO foo1 (id) VALUES (1)"

setupCache :: Bool -> IO Cache
setupCache cow = do
  cacheInfo <- setupInitDbCache (defaultCacheConfig { cacheUseCopyOnWrite = cow})
  void (withConfig (defaultConfig <> cacheConfig cacheInfo) (const $ pure ()))
  pure cacheInfo

setupWithCache :: (Config -> Benchmark) -> Benchmark
setupWithCache f = envWithCleanup (setupCache True) cleanupInitDbCache $ f . (defaultConfig <>) . cacheConfig

setupWithCacheNoCow :: (Config -> Benchmark) -> Benchmark
setupWithCacheNoCow f = envWithCleanup (setupCache False) cleanupInitDbCache $ f . (defaultConfig <>) . cacheConfig

setupCacheAndSP :: IO (Cache, Snapshot, Once Config)
setupCacheAndSP = do
  cacheInfo <- setupCache True
  let theCacheConfig = defaultConfig <> cacheConfig cacheInfo
  sp <- either throwIO pure <=< withConfig theCacheConfig $ \db -> do
    migrateDb db
    either throwIO pure =<< takeSnapshot db

  let theConfig = defaultConfig <> snapshotConfig sp <> theCacheConfig

  pure (cacheInfo, sp, Once theConfig)

cleanupCacheAndSP :: (Cache, Snapshot, Once Config) -> IO ()
cleanupCacheAndSP (x, y, _) = cleanupSnapshot y >> cleanupInitDbCache x

setupWithCacheAndSP :: (Config -> Benchmark) -> Benchmark
setupWithCacheAndSP f = envWithCleanup setupCacheAndSP cleanupCacheAndSP $ \ ~(_, _, Once x) -> f x

setupWithCacheAndSP' :: (Snapshot -> Benchmark) -> Benchmark
setupWithCacheAndSP' f = envWithCleanup setupCacheAndSP cleanupCacheAndSP $ \ ~(_, x, _) -> f x

setupCacheAndAction :: IO (Cache, FilePath, Once Config)
setupCacheAndAction = do
  cacheInfo <- setupCache True
  snapshotDir <- createTempDirectory "/tmp" "tmp-postgres-bench-cache"
  let theCacheConfig = defaultConfig <> cacheConfig cacheInfo

  theConfig <- either throwIO pure =<< cacheAction snapshotDir migrateDb theCacheConfig

  pure (cacheInfo, snapshotDir, Once theConfig)

cleanupCacheAndAction :: (Cache, FilePath, Once Config) -> IO ()
cleanupCacheAndAction (c, f, _) = rmDirIgnoreErrors f >> cleanupInitDbCache c

setupWithCacheAndAction :: (FilePath -> Config -> Benchmark) -> Benchmark
setupWithCacheAndAction f = envWithCleanup setupCacheAndAction cleanupCacheAndAction $
  \ ~(_, filePath, Once x) -> f filePath x

main :: IO ()
main = defaultMain
  [ bench "with" $ whnfIO $ with $ const $ pure ()

  , bench "withConfig no --no-sync" $ whnfIO $
      withConfig defaultConfigDefaultInitDb $ const $ pure ()

  , bench "withConfig verbose" $ whnfIO $
      withConfig verboseConfig $ const $ pure ()

  , bench "withConfig db create" $ whnfIO $
      withConfig (optionsToDefaultConfig (mempty { Options.dbname = pure "test" } )) $
        const $ pure ()

  , setupWithCacheNoCow $ \theConfig -> bench "withConfig silent cache no cow" $ whnfIO $
      withConfig theConfig $ const $ pure ()

  , setupWithCache $ \theCacheConfig -> bench "withConfig silent cache" $ whnfIO $
      withConfig theCacheConfig $ const $ pure ()

  , bench "with migrate 10x" $ whnfIO $ replicateM 10 $ withConfig defaultConfig $ \db ->
      migrateDb db >> testQuery db
{-
  , setupWithCache $ \theCacheConfig -> do
      bench "with migrate 10x and cache" $ whnfIO $ withConfig theCacheConfig $ \_ -> do
        replicateM_ 10 $ withConfig theCacheConfig $ \db ->
          migrateDb db >> testQuery db

  , setupWithCache $ \theCacheConfig -> bench "withSnapshot migrate 10x and cache" $ whnfIO $ withConfig theCacheConfig $ \db -> do
      migrateDb db
      void $ withSnapshot db $ \theSnapshotDir -> do
        let theSnapshotConfig = defaultConfig <> snapshotConfig theSnapshotDir
        replicateM_ 10 $ withConfig theSnapshotConfig testQuery

  , setupWithCache $ \theCacheConfig -> bench "cache action and recache and cache" $ whnfIO $ withTempDirectory "/tmp" "tmp-postgres-bench-cache" $ \snapshotDir -> do
      newConfig <- either throwIO pure =<< cacheAction snapshotDir migrateDb theCacheConfig
      replicateM_ 10 $
        either throwIO pure =<< flip withConfig testQuery
          =<< either throwIO pure =<< cacheAction snapshotDir migrateDb newConfig

  , setupWithCacheAndAction $ \snapshotDir theCacheConfig -> bench "pre-cache action and recache" $ whnfIO $ do
      replicateM_ 10 $
        either throwIO pure =<< flip withConfig testQuery
          =<< either throwIO pure =<< cacheAction snapshotDir migrateDb theCacheConfig
-}
  , setupWithCacheAndSP $ \theConfig -> bench "withConfig pre-setup with withSnapshot" $ whnfIO $
      void $ withConfig theConfig $ const $ pure ()

  , setupWithCacheAndSP' $ \sp -> bench "snapshotConfig" $ whnfIO $ void $ flip withConfig (const $ pure ())
    $ snapshotConfig sp

  , bench "migrateDb" $ perRunEnvWithCleanup (either throwIO (pure . Once) =<< startConfig defaultConfig) (stop . unOnce) $
      \ ~(Once db) -> migrateDb db

  , bench "withSnapshot" $ perRunEnvWithCleanup (either throwIO (pure . Once) =<< startConfig defaultConfig) (stop . unOnce) $
      \ ~(Once db) -> void $ withSnapshot db $ const $ pure ()

  , bench "stopGracefully" $ perRunEnvWithCleanup (either throwIO (pure . Once) =<< start) (stop . unOnce) $
    \ ~(Once db) -> do
      void $ stopPostgresGracefully db
      stop db

  , bench "stop" $ perRunEnvWithCleanup (either throwIO (pure . Once) =<< start) (stop . unOnce) $
      \ ~(Once db) -> stop db

  , bench "stop serial" $ perRunEnvWithCleanup (either throwIO (pure . Once) =<< start) (stop . unOnce) $
      \ ~(Once DB {..}) ->
          stopPlan dbPostgresProcess >> cleanupConfig dbResources
  ]
