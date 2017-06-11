{-# LANGUAGE OverloadedStrings #-}
module Database.Postgres.Temp.InternalSpec (main, spec) where
import Test.Hspec
import System.IO.Temp
import Database.Postgres.Temp.Internal
import Data.Typeable
import Control.Exception
import System.IO
import System.Directory
import Control.Monad
import System.Process
import Database.PostgreSQL.Simple
import qualified Data.ByteString.Char8 as BSC

main :: IO ()
main = hspec spec

mkDevNull :: IO Handle
mkDevNull = openFile "/dev/null" WriteMode

data Except = Except
  deriving (Show, Eq, Typeable)

instance Exception Except

spec :: Spec
spec = describe "Database.Postgres.Temp.Internal" $ do
  before (createTempDirectory "/tmp" "tmp-postgres") $ after rmDirIgnoreErrors $ describe "startWithLogger/stop" $ do
    it "deletes the temp dir and postgres on exception" $ \mainFilePath -> do
      -- This is not the best method ... but it works
      beforePostgresCount <- length . lines <$> readProcess "pgrep" ["postgres"] []
      stdOut <- mkDevNull
      stdErr <- mkDevNull
      forM_ [minBound .. maxBound] $ \event -> do
        shouldThrow
          (startWithLogger (\currentEvent -> when (currentEvent == event) $ throwIO Except) mainFilePath stdOut stdErr)
          (\Except -> True)
        doesDirectoryExist mainFilePath `shouldReturn` False
        (length . lines <$> readProcess "pgrep" ["postgres"] []) `shouldReturn` beforePostgresCount

    it "creates a useful connection string and stop still cleans up" $ \mainFilePath -> do
      beforePostgresCount <- length . lines <$> readProcess "pgrep" ["postgres"] []
      stdOut <- mkDevNull
      stdErr <- mkDevNull
      Right db <- startWithLogger (\_ -> return ()) mainFilePath stdOut stdErr
      conn <- connectPostgreSQL $ BSC.pack $ connectionString db
      execute_ conn "create table users (id int)"

      stop 10000000 db `shouldReturn` Success
      doesDirectoryExist mainFilePath `shouldReturn` False
      (length . lines <$> readProcess "pgrep" ["postgres"] []) `shouldReturn` beforePostgresCount

    it "stop will kill if shutdown takes too long" $ \mainFilePath -> do
      beforePostgresCount <- length . lines <$> readProcess "pgrep" ["postgres"] []
      stdOut <- mkDevNull
      stdErr <- mkDevNull
      Right db <- startWithLogger (\_ -> return ()) mainFilePath stdOut stdErr
      stop 0 db `shouldReturn` TimedOut 0

      doesDirectoryExist mainFilePath `shouldReturn` False
      (length . lines <$> readProcess "pgrep" ["postgres"] []) `shouldReturn` beforePostgresCount


