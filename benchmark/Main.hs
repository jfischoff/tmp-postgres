module Main where
import Control.Exception
import Control.Monad (void, replicateM)
import Criterion.Main
import Data.String
import Database.Postgres.Temp.Internal
import Database.Postgres.Temp.Internal.Config
import qualified Database.PostgreSQL.Simple as PG

defaultConfigDefaultInitDb :: Config
defaultConfigDefaultInitDb = mempty
  { plan = mempty
    { logger = pure mempty
    , postgresConfigFile = defaultPostgresConfig
    , createDbConfig = Nothing
    , initDbConfig = pure mempty
    }
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

setupCache :: IO (Bool, CompleteDirectoryType)
setupCache = do
  defCacheConfig <- createDefaultCacheConfig
  cacheInfo <- setupInitDbCache defCacheConfig
  void (withConfig (silentConfig <> toCacheConfig cacheInfo) (const $ pure ()))
  pure cacheInfo

setupWithCache :: (Config -> Benchmark) -> Benchmark
setupWithCache f = envWithCleanup setupCache cleanupInitDbCache $ f . (silentConfig <>) . toCacheConfig

main :: IO ()
main = defaultMain
  [ --bench "with" $ whnfIO $ with $ const $ pure ()
  --, bench "withConfig no --no-sync" $ whnfIO $
  --    withConfig defaultConfigDefaultInitDb $ const $ pure ()
  bench "withConfig silent" $ whnfIO $
    withConfig silentConfig $ const $ pure ()

  , setupWithCache $ \cacheConfig -> bench "withConfig silent cache" $ whnfIO $
      withConfig cacheConfig $ const $ pure ()

  , bench "with migrate 10x" $ whnfIO $ replicateM 10 $ withConfig silentConfig $ \db ->
      migrateDb db >> testQuery db

  , bench "withNewDb migrate 10x" $ whnfIO $ withConfig silentConfig $ \db -> do
      migrateDb db
      replicateM 10 $ withNewDb db testQuery

  , setupWithCache $ \cacheConfig -> do
      bench "with migrate 10x and cache" $ whnfIO $ withConfig cacheConfig $ \_ -> do
        replicateM 10 $ withConfig cacheConfig $ \db ->
          migrateDb db >> testQuery db

  , setupWithCache $ \cacheConfig -> bench "withNewDbConfig migrate 10x and cache" $ whnfIO $ withConfig cacheConfig
    $ \db -> do
      migrateDb db
      replicateM 10 $ withNewDb db testQuery
  ]
