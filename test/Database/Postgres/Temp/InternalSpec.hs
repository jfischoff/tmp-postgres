{-# LANGUAGE OverloadedStrings, DeriveDataTypeable, QuasiQuotes, ScopedTypeVariables, LambdaCase #-}
module Database.Postgres.Temp.InternalSpec where
import Test.Hspec
-- import System.IO.Temp
import Database.Postgres.Temp.Internal
-- import Data.Typeable
import Control.Exception
-- import System.IO
-- import System.Directory
-- import Control.Monad
import System.Process
-- import Database.PostgreSQL.Simple
-- import qualified Data.ByteString.Char8 as BSC
import System.Exit
import qualified Database.PostgreSQL.Simple as PG
-- import System.Timeout(timeout)
-- import Data.Either
-- import Data.Function (fix)
-- import Control.Concurrent

-- What are the properties of startWith/stop?
-- TODO
-- I should check for various exes on the path and complain with better error messages if they are not there

-- postgres should be running after start
-- postgres should not be running after stop
-- if initdb is called it should make database folders if
-- in a clean directory
-- If initdb is not called the folder should be unmodified
-- if a temp folder is used it should be created and cleaned up
-- if a temp port is used it should be free
-- Once the db is returned we should be able to connect to it
-- everything should be exceptions safe
-- adding command line arguments works
-- adding config works
-- If there is initdb/createdb plan there are options
-- and they should match

-- Some usage tests
-- creating a db with a specific user and db works
-- backup stuff works

spec :: Spec
spec = describe "Database.Postgres.Temp.Internal" $ do
  it "start/stop the postgres process is running and then it is not" $ do
    bracket start (either mempty stop)   $ \result -> do

      db@DB {..} <- case result of
        Left err -> throwIO err
        Right x -> pure x
      getProcessExitCode (pid dbPostgresProcess) `shouldReturn` Nothing

      stop db

      getProcessExitCode (pid dbPostgresProcess) `shouldReturn` Just ExitSuccess

  it "Can connect to the db after it starts" $ do
    Right one <- with $ \db -> do
      fmap (PG.fromOnly . head) $ bracket (PG.connectPostgreSQL $ toConnectionString db ) PG.close $
        \conn -> PG.query_ conn "SELECT 1"

    one `shouldBe` (1 :: Int)



{-
mkDevNull :: IO Handle
mkDevNull = openFile "/dev/null" WriteMode

data Except = Except
  deriving (Show, Eq, Typeable)

instance Exception Except

countPostgresProcesses :: IO Int
countPostgresProcesses = do
  (exitCode, xs, _) <-  readProcessWithExitCode "pgrep" ["postgres"] []

  unless (exitCode == ExitSuccess || exitCode == ExitFailure 1) $ throwIO exitCode

  pure $ length $ lines xs


  before (createTempDirectory "/tmp" "tmp-postgres") $ after rmDirIgnoreErrors $ describe "startWithLogger/stop" $ do
    forM_ [minBound .. maxBound] $ \event ->
      it ("deletes the temp dir and postgres on exception in " ++ show event) $ \mainFilePath -> do
        -- This is not the best method ... but it works
        beforePostgresCount <- countPostgresProcesses
        theStdOut <- mkDevNull
        theStdErr <- mkDevNull
        shouldThrow
          (startWithLogger (\currentEvent -> when (currentEvent == event) $ throwIO Except) Unix defaultOptions mainFilePath theStdOut theStdErr)
          (\Except -> True)
        doesDirectoryExist mainFilePath `shouldReturn` False
        countPostgresProcesses `shouldReturn` beforePostgresCount

    it "creates a useful connection string and stop still cleans up" $ \mainFilePath -> do
      beforePostgresCount <- countPostgresProcesses
      theStdOut <- mkDevNull
      theStdErr <- mkDevNull
      result <- startWithLogger (\_ -> return ()) Unix defaultOptions mainFilePath theStdOut theStdErr
      db <- case result of
              Right x  -> return x
              Left err -> error $ show err
      conn <- connectPostgreSQL $ BSC.pack $ connectionString db
      _ <- execute_ conn "create table users (id int)"

      stop db `shouldReturn` Just ExitSuccess
      doesDirectoryExist mainFilePath `shouldReturn` False
      countPostgresProcesses `shouldReturn` beforePostgresCount

    it "can override settings" $ \mainFilePath -> do
      let expectedDuration = "100ms"
      theStdOut <- mkDevNull
      theStdErr <- mkDevNull
      bracket (startWithLogger (const $ pure ()) Unix
                defaultOptions { tmpCmdLineOptions = [("log_min_duration_statement", expectedDuration)] }
                mainFilePath theStdOut theStdErr
                )
              (either (\_ -> return ()) (void . stop)) $ \result -> do
        db <- case result of
                Right x  -> return x
                Left err -> error $ show err
        conn <- connectPostgreSQL $ BSC.pack $ connectionString db
        [Only actualDuration] <- query_ conn "SHOW log_min_duration_statement"
        actualDuration `shouldBe` expectedDuration

    it "dies promptly when a bad setting is passed" $ \mainFilePath -> do
      theStdOut <- mkDevNull
      theStdErr <- mkDevNull
      r <- timeout 5000000 $ startWithLogger (const $ pure ()) Unix
            defaultOptions { tmpCmdLineOptions =  [ ("log_directory", "/this/does/not/exist")
            , ("logging_collector", "true")
            ] } mainFilePath theStdOut theStdErr
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
      theStdOut <- mkDevNull
      theStdErr <- mkDevNull
      bracket (fromRight (error "failed to start db") <$> startWithLogger (\_ -> return ()) Unix defaultOptions mainFilePath theStdOut theStdErr) stop $ \db -> do
        bracket (connectPostgreSQL $ BSC.pack $ connectionString db) close $ \_ ->
          bracket (connectPostgreSQL $ BSC.pack $ connectionString db) close $ \conn2 -> do
            query_ conn2 "SELECT COUNT(*) FROM pg_stat_activity where backend_type='client backend'" `shouldReturn` [Only (2 :: Int)]

            terminateConnections db

            bracket (connectPostgreSQL $ BSC.pack $ connectionString db) close $ \conn3 ->
              query_ conn3 "SELECT COUNT(*) FROM  pg_stat_activity where backend_type='client backend'" `shouldReturn` [Only (1 :: Int)]

    -- The point of stopPostgres and startPostgres is to test recovery so
    -- let's make sure that works
    it "stopPostgres/startPostgres works" $ \mainFilePath -> do
      let extraOpts = defaultOptions { tmpCmdLineOptions =
            [ ("wal_level", "replica")
            , ("archive_mode", "on")
            , ("max_wal_senders", "2")
            , ("fsync", "on")
            , ("synchronous_commit", "on")
            ]
            }

      theStdOut <- mkDevNull
      theStdErr <- mkDevNull

      bracket (fromRight (error "failed to start db") <$> startWithLogger (\_ -> return ()) Unix extraOpts mainFilePath theStdOut theStdErr) stop $ \db -> do
        bracket (connectPostgreSQL $ BSC.pack $ connectionString db) close $ \conn -> do
          let dataDir = mainFilePath ++ "/data"
              walArchiveDir = mainFilePath ++ "/archive"
              baseBackupFile = mainFilePath ++ "/backup"

          appendFile (dataDir ++ "/pg_hba.conf") $ "local replication all trust"
          let archiveLine = "archive_command = " ++
                "'test ! -f " ++ walArchiveDir ++ "/%f && cp %p " ++ walArchiveDir ++ "/%f'\n"

          appendFile (dataDir ++ "/postgresql.conf") $ archiveLine

          createDirectory walArchiveDir

          reloadConfig db

          res <- system ("pg_basebackup -D " ++ baseBackupFile ++ " --format=tar -p" ++ show (port db) ++ " -h" ++ mainFilePath)
          res `shouldBe` ExitSuccess

          _ <- execute_ conn "CREATE TABLE foo(id int PRIMARY KEY);"
          _ <- execute_ conn "BEGIN ISOLATION LEVEL READ COMMITTED READ WRITE; INSERT INTO foo (id) VALUES (1); COMMIT"
          _ :: [Only String] <- query_ conn "SELECT pg_walfile_name(pg_switch_wal())"
          _ :: [Only String] <- query_ conn "SELECT pg_walfile_name(pg_create_restore_point('pitr'))"
          _ <- execute_ conn "BEGIN ISOLATION LEVEL READ COMMITTED READ WRITE; INSERT INTO foo (id) VALUES (2); COMMIT"

          query_ conn "SELECT id FROM foo ORDER BY id ASC"
            `shouldReturn` [Only (1 :: Int), Only 2]

          close conn

          stopPostgres db `shouldReturn` Just ExitSuccess

          removeDirectoryRecursive dataDir
          createDirectory dataDir
          let untarCommand = "tar -C" ++ dataDir ++ " -xf " ++ baseBackupFile ++ "/base.tar"
          system untarCommand `shouldReturn` ExitSuccess
          system ("chmod -R 700 " ++ dataDir) `shouldReturn` ExitSuccess
          writeFile (dataDir ++ "/recovery.conf") $ "recovery_target_name='pitr'\nrecovery_target_action='promote'\nrecovery_target_inclusive=true\nrestore_command='"
             ++ "cp " ++ walArchiveDir ++ "/%f %p'"

          startPostgres db
          bracket (connectPostgreSQL $ BSC.pack $ connectionString db) close $ \conn1 -> do
            fix $ \next -> do
              fmap (fromOnly . head) (query_ conn1 "SELECT pg_is_in_recovery()") >>= \case
                True -> threadDelay 100000 >> next
                False -> pure ()

            query_ conn1 "SELECT id FROM foo ORDER BY id ASC"
              `shouldReturn` [Only (1 :: Int)]
-}