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
import Data.Either

main :: IO ()
main = hspec spec

mkDevNull :: IO Handle
mkDevNull = openFile "/dev/null" WriteMode

data Except = Except
  deriving (Show, Eq, Typeable)

instance Exception Except

countPostgresProcesses :: IO Int
countPostgresProcesses = length . lines <$> readProcess "pgrep" ["postgres"] []

spec :: Spec
spec = describe "Database.Postgres.Temp.Internal" $ do
  before (createTempDirectory "/tmp" "tmp-postgres") $ after rmDirIgnoreErrors $ describe "startWithLogger/stop" $ do
    forM_ [minBound .. maxBound] $ \event ->
      it ("deletes the temp dir and postgres on exception in " ++ show event) $ \mainFilePath -> do
        -- This is not the best method ... but it works
        beforePostgresCount <- countPostgresProcesses
        let stdOut = stdout
            stdErr = stderr
        shouldThrow
          (startWithLogger (\currentEvent -> when (currentEvent == event) $ throwIO Except) mainFilePath stdOut stdErr)
          (\Except -> True)
        doesDirectoryExist mainFilePath `shouldReturn` False
        countPostgresProcesses `shouldReturn` beforePostgresCount

    it "creates a useful connection string and stop still cleans up" $ \mainFilePath -> do
      beforePostgresCount <- countPostgresProcesses
      stdOut <- mkDevNull
      stdErr <- mkDevNull
      result <- startWithLogger (\_ -> return ()) mainFilePath stdOut stdErr
      db <- case result of
              Right x  -> return x
              Left err -> error $ show err
      conn <- connectPostgreSQL $ BSC.pack $ connectionString db
      execute_ conn "create table users (id int)"

      stop db `shouldReturn` Success
      doesDirectoryExist mainFilePath `shouldReturn` False
      countPostgresProcesses `shouldReturn` beforePostgresCount

    it "stopWithConnectionString works" $ \_ -> do
      beforePostgresCount <- countPostgresProcesses
      Right db <- startAndLogToTmp
      stopWithConnectionString (connectionString db) `shouldReturn` Success

      countPostgresProcesses `shouldReturn` beforePostgresCount
