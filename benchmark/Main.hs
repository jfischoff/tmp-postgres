module Main where
import Control.DeepSeq
import Control.Exception
import Control.Monad
import Criterion.Main hiding (defaultConfig)
import Data.String
import Database.Postgres.Temp.Internal
import Database.Postgres.Temp.Internal.Config
import qualified Database.PostgreSQL.Simple as PG

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

setupCache :: IO CacheResources
setupCache = do
  cacheInfo <- setupInitDbCache defaultCacheConfig
  void (withConfig (defaultConfig <> cacheResourcesToConfig cacheInfo) (const $ pure ()))
  pure cacheInfo

setupWithCache :: (Config -> Benchmark) -> Benchmark
setupWithCache f = envWithCleanup setupCache cleanupInitDbCache $ f . (defaultConfig <>) . cacheResourcesToConfig

setupCacheAndSP :: IO (CacheResources, Snapshot, Once Config)
setupCacheAndSP = do
  cacheInfo <- setupCache
  let cacheConfig = defaultConfig <> cacheResourcesToConfig cacheInfo
  sp <- either throwIO pure <=< withConfig cacheConfig $ \db -> do
    migrateDb db
    either throwIO pure =<< takeSnapshot Temporary db

  let theConfig = defaultConfig <> snapshotConfig sp <> cacheConfig


  pure (cacheInfo, sp, Once theConfig)

cleanupCacheAndSP :: (CacheResources, Snapshot, Once Config) -> IO ()
cleanupCacheAndSP (x, y, _) = cleanupSnapshot y >> cleanupInitDbCache x

setupWithCacheAndSP :: (Config -> Benchmark) -> Benchmark
setupWithCacheAndSP f = envWithCleanup setupCacheAndSP cleanupCacheAndSP $ \ ~(_, _, Once x) -> f x

setupWithCacheAndSP' :: (Snapshot -> Benchmark) -> Benchmark
setupWithCacheAndSP' f = envWithCleanup setupCacheAndSP cleanupCacheAndSP $ \ ~(_, x, _) -> f x

main :: IO ()
main = defaultMain
  [ --bench "with" $ whnfIO $ with $ const $ pure ()
  --, bench "withConfig no --no-sync" $ whnfIO $
  --    withConfig defaultConfigDefaultInitDb $ const $ pure ()
{-
  bench "withConfig silent" $ whnfIO $
    withConfig defaultConfig $ const $ pure ()

  , setupWithCache $ \cacheConfig -> bench "withConfig silent cache" $ whnfIO $
      withConfig cacheConfig $ const $ pure ()

  , bench "with migrate 10x" $ whnfIO $ replicateM 10 $ withConfig defaultConfig $ \db ->
      migrateDb db >> testQuery db

-}

    setupWithCache $ \cacheConfig -> do
      bench "with migrate 10x and cache" $ whnfIO $ withConfig cacheConfig $ \_ -> do
        replicateM_ 10 $ withConfig cacheConfig $ \db ->
          migrateDb db >> testQuery db

  , setupWithCache $ \cacheConfig -> bench "withSnapshot migrate 10x and cache" $ whnfIO $ withConfig cacheConfig $ \db -> do
      migrateDb db
      void $ withSnapshot Temporary db $ \snapshotDir -> do
        let theSnapshotConfig = defaultConfig <> snapshotConfig snapshotDir
        replicateM_ 10 $ withConfig theSnapshotConfig testQuery
{-
  , setupWithCacheAndSP $ \theConfig -> bench "withConfig pre-setup with withSnapshot" $ whnfIO $
      void $ withConfig theConfig $ const $ pure ()

  , setupWithCacheAndSP' $ \sp -> bench "snapshotConfig" $ whnfIO $ void $ snapshotConfig $ toFilePath sp

  , bench "migrateDb" $ perRunEnvWithCleanup (either throwIO (pure . Once) =<< startConfig defaultConfig) (stop . unOnce) $
      \ ~(Once db) -> migrateDb db
-}
  , bench "withSnapshot" $ perRunEnvWithCleanup (either throwIO (pure . Once) =<< startConfig defaultConfig) (stop . unOnce) $
      \ ~(Once db) -> void $ withSnapshot Temporary db $ const $ pure ()

  ]
