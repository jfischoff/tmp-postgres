module Main where
import Control.Exception
import Control.Monad (void, replicateM)
import Criterion.Main
import Data.String
import Database.Postgres.Temp.Internal
import Database.Postgres.Temp.Config
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

main :: IO ()
main = defaultMain
  [ --bench "with" $ whnfIO $ with $ const $ pure ()
  --, bench "withConfig no --no-sync" $ whnfIO $
  --    withConfig defaultConfigDefaultInitDb $ const $ pure ()
  bench "withConfig silent" $ whnfIO $
      withConfig silentConfig $ const $ pure ()
  --  bench "with migrate 10x" $ whnfIO $ replicateM 10 $ withConfig silent $ \db ->
  --    migrateDb db >> testQuery db
  --, bench "withNewDb migrate 10x" $ whnfIO $ withConfig silent $ \db -> do
  --    migrateDb db
  --    replicateM 10 $ withNewDb db testQuery
  ]
