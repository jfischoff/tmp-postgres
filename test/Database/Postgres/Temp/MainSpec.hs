{-# LANGUAGE OverloadedStrings #-}
module Database.Postgres.Temp.MainSpec (main, spec) where
import Test.Hspec
import qualified Database.Postgres.Temp.Main as Temp
import System.Process
import Database.PostgreSQL.Simple
import Data.IORef
import qualified Data.ByteString.Char8 as BSC
import System.Exit


main :: IO ()
main = hspec spec

spec :: Spec
spec = beforeAll (newIORef "") $ describe "Database.Postgres.Temp.Main" $ do
  it "creates a connection string with no args" $ \ref -> do
    connectionString <- readProcess "stack" ["exec", "tmp-postgres"] []
    conn <- connectPostgreSQL $ BSC.pack $ connectionString
    execute_ conn "create table users (id int)"
    writeIORef ref connectionString
  it "cleans up when passed a connection string" $ \ref -> do
    connectionString <- readIORef ref
    system ("stack exec tmp-postgres -- " ++ connectionString) `shouldReturn` ExitSuccess




