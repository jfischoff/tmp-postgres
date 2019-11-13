module Main where
import Criterion.Main
import Database.Postgres.Temp.Internal
import Database.Postgres.Temp.Config

defaultConfigDefaultInitDb :: Config
defaultConfigDefaultInitDb = mempty
  { plan = mempty
    { logger = pure mempty
    , postgresConfigFile = defaultPostgresConfig
    , createDbConfig = Nothing
    , initDbConfig = pure mempty
    }
  }

main :: IO ()
main = defaultMain
  [ bench "with" $ whnfIO $ with $ const $ pure ()
  , bench "withConfig no --no-sync" $ whnfIO $
      withConfig defaultConfigDefaultInitDb $ const $ pure ()
  , bench "withConfig silent" $ whnfIO $
      withConfig silentConfig $ const $ pure ()
  ]
