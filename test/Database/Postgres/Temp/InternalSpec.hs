{-# LANGUAGE OverloadedStrings, DeriveDataTypeable #-}
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
import System.Exit
import System.Timeout(timeout)
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
        stdOut <- mkDevNull
        stdErr <- mkDevNull
        shouldThrow
          (startWithLogger (\currentEvent -> when (currentEvent == event) $ throwIO Except) Unix [] mainFilePath stdOut stdErr)
          (\Except -> True)
        doesDirectoryExist mainFilePath `shouldReturn` False
        countPostgresProcesses `shouldReturn` beforePostgresCount

    it "creates a useful connection string and stop still cleans up" $ \mainFilePath -> do
      beforePostgresCount <- countPostgresProcesses
      stdOut <- mkDevNull
      stdErr <- mkDevNull
      result <- startWithLogger (\_ -> return ()) Unix [] mainFilePath stdOut stdErr
      db <- case result of
              Right x  -> return x
              Left err -> error $ show err
      conn <- connectPostgreSQL $ BSC.pack $ connectionString db
      _ <- execute_ conn "create table users (id int)"

      stop db `shouldReturn` ExitSuccess
      doesDirectoryExist mainFilePath `shouldReturn` False
      countPostgresProcesses `shouldReturn` beforePostgresCount

    it "can override settings" $ \mainFilePath -> do
      let expectedDuration = "100ms"
      stdOut <- mkDevNull
      stdErr <- mkDevNull
      bracket (startWithLogger (const $ pure ()) Unix
                [("log_min_duration_statement", expectedDuration)]
                mainFilePath stdOut stdErr
                )
              (either (\_ -> return ()) (void . stop)) $ \result -> do
        db <- case result of
                Right x  -> return x
                Left err -> error $ show err
        conn <- connectPostgreSQL $ BSC.pack $ connectionString db
        [Only actualDuration] <- query_ conn "SHOW log_min_duration_statement"
        actualDuration `shouldBe` expectedDuration

    it "dies promptly when a bad setting is passed" $ \mainFilePath -> do
      stdOut <- mkDevNull
      stdErr <- mkDevNull
      r <- timeout 5000000 $ startWithLogger (const $ pure ()) Unix
            [ ("log_directory", "/this/does/not/exist")
            , ("logging_collector", "true")
            ] mainFilePath stdOut stdErr
      case r of
        Nothing ->
          -- bad test, shouldSatisfy is difficult because it wants Show on DB.
          -- anyway, point of this is to fail if we timed out.
          1 `shouldBe` (2 :: Int)
        Just (Right x) ->
          -- this would be very surprising but if it somehow manages to do something useful despite
          -- bad config ... ok i guess? regardless, should clean up.
          void $ stop x
        Just (Left _) -> do
          -- if it fails here that's fine & expected.
          pure ()

    it "terminateConnections" $ \mainFilePath -> do
      stdOut <- mkDevNull
      stdErr <- mkDevNull
      bracket (fromRight (error "failed to start db") <$> startWithLogger (\_ -> return ()) Unix [] mainFilePath stdOut stdErr) stop $ \db -> do
        bracket (connectPostgreSQL $ BSC.pack $ connectionString db) close $ \_ ->
          bracket (connectPostgreSQL $ BSC.pack $ connectionString db) close $ \conn2 -> do
            query_ conn2 "SELECT COUNT(*) FROM pg_stat_activity" `shouldReturn` [Only (2 :: Int)]

            terminateConnections db

            bracket (connectPostgreSQL $ BSC.pack $ connectionString db) close $ \conn3 ->
              query_ conn3 "SELECT COUNT(*) FROM  pg_stat_activity" `shouldReturn` [Only (1 :: Int)]
