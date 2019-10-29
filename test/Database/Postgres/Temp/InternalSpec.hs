{-# LANGUAGE OverloadedStrings, DeriveDataTypeable, QuasiQuotes, ScopedTypeVariables, LambdaCase, RankNTypes #-}
module Database.Postgres.Temp.InternalSpec where
import Test.Hspec
import Database.Postgres.Temp.Core
import Database.Postgres.Temp.Partial
import Database.Postgres.Temp.Internal
-- import Data.Typeable
import Control.Exception
import System.IO.Error
-- import System.Directory
-- import Control.Monad
import System.Process
-- import Database.PostgreSQL.Simple
-- import qualified Data.ByteString.Char8 as BSC
import System.Exit
import qualified Database.PostgreSQL.Simple as PG
import System.Environment
import System.Posix.Files
import System.IO.Temp
import System.Directory
import qualified Database.PostgreSQL.Simple.Options as PostgresClient
import qualified Database.PostgreSQL.Simple.PartialOptions as Client
import Data.String
import System.Timeout(timeout)
import Control.Monad (void)
-- import Data.Either
-- import Data.Function (fix)
-- import Control.Concurrent

-- What are the properties of startWith/stop?

-- everything should be exceptions safe

-- Some usage tests
-- backup stuff works

newtype Runner =  Runner { unRunner :: forall a. (DB -> IO a) -> IO a }

withRunner :: (DB -> IO ()) -> Runner -> IO ()
withRunner g (Runner f) = f g

updateCreateDb :: PartialResources -> Lastoid (Maybe PartialProcessOptions) -> PartialResources
updateCreateDb partialResources partialProcessOptions =
  let originalPlan = partialResourcesPlan partialResources
      newPlan = originalPlan
        { partialPlanCreateDb = partialProcessOptions
        }
  in partialResources
      { partialResourcesPlan = newPlan
      }

defaultOptionsShouldMatchDefaultPlan :: SpecWith Runner
defaultOptionsShouldMatchDefaultPlan =
  it "default options should match default plan" $ withRunner $ \DB{..} -> do
    let Resources {..} = dbResources
        Plan {..} = resourcesPlan
        PostgresPlan {..} = planPostgres
    PostgresClient.oDbname postgresPlanClientOptions `shouldBe` "test"
    let Temporary tmpDataDir = resourcesDataDir
    tmpDataDir `shouldStartWith` "/tmp/tmp-postgres-data"
    let Just port = PostgresClient.oPort postgresPlanClientOptions
    port `shouldSatisfy` (>32768)
    let UnixSocket (Temporary unixSocket) = resourcesSocket
    unixSocket `shouldStartWith` "/tmp/tmp-postgres-socket"
    postgresPlanClientOptions `shouldBe`
      ((PostgresClient.defaultOptions (PostgresClient.oDbname postgresPlanClientOptions))
        { PostgresClient.oPort = PostgresClient.oPort postgresPlanClientOptions
        , PostgresClient.oHost = PostgresClient.oHost postgresPlanClientOptions
        })

customOptionsWork :: (PartialResources -> (DB -> IO ()) -> IO ()) -> Spec
customOptionsWork action = do
  let expectedDbName = "thedb"
      expectedPassword = "password"
      expectedUser = "user-name"
      expectedDuration = "100ms"
      extraConfig = "log_min_duration_statement='" <> expectedDuration <> "'"

  it "returns the right client options for the plan" $ do
    initialPlan <- defaultPartialResources
    initialCreateDbOptions <- standardProcessOptions
    let customPlan = mempty
          { partialResourcesPlan = mempty
              { partialPlanPostgres = mempty
                  { partialPostgresPlanClientOptions = mempty
                      { Client.user     = pure expectedUser
                      , Client.password = pure expectedPassword
                      , Client.dbname   = pure expectedDbName
                      }
                  }
              , partialPlanInitDb = Mappend $ pure $ mempty
                  { partialProcessOptionsCmdLine = Mappend
                      ["--user", "user-name"
                      ]
                  , partialProcessOptionsEnvVars = Mappend
                      [ ("PGPASSWORD", "password")
                      ]
                  }
              , partialPlanConfig = Mappend [extraConfig]
              }
          }
    -- hmm maybe I should provide lenses
    let combinedResources = initialPlan <> customPlan
        finalCombinedResources = updateCreateDb combinedResources $ Mappend $ pure $ initialCreateDbOptions
          { partialProcessOptionsCmdLine = Mappend
              ["--user", "user-name"
              , expectedDbName
              ]
          , partialProcessOptionsEnvVars = Mappend
              [ ("PGPASSWORD", "password")
              ]
          }

    action finalCombinedResources $ \db@DB {..} -> do
      bracket (PG.connectPostgreSQL $ toConnectionString db) PG.close $ \conn -> do
        [PG.Only actualDuration] <- PG.query_ conn "SHOW log_min_duration_statement"
        actualDuration `shouldBe` expectedDuration

      let Resources {..} = dbResources
          Plan {..} = resourcesPlan
          actualOptions = postgresPlanClientOptions planPostgres
          actualConfig = planConfig
      PostgresClient.oUser actualOptions `shouldBe` Just expectedUser
      PostgresClient.oDbname actualOptions `shouldBe` expectedDbName
      PostgresClient.oPassword actualOptions `shouldBe` Just expectedPassword
      lines actualConfig `shouldContain` defaultConfig <> [extraConfig]

invalidOptionsFailsQuickly :: (PartialResources -> IO ()) -> Spec
invalidOptionsFailsQuickly action = it "quickly fails with an invalid option" $ do
  initialPlan <- defaultPartialResources
  let customPlan = mempty
        { partialResourcesPlan = mempty
            { partialPlanConfig = Mappend
                [ "log_directory = /this/does/not/exist"
                , "logging_collector = true"
                ]
            }
        }
  timeout 5000000 (action $ initialPlan <> customPlan) `shouldThrow`
    (== StartPostgresFailed (ExitFailure 1))


throwsIfCreateDbIsNotOnThePath :: IO a -> Spec
throwsIfCreateDbIsNotOnThePath action = it "throws if createdb is not on the path" $
  withSystemTempDirectory "createdb-not-on-path-test" $ \dir -> do
    Just initDbPath   <- findExecutable "initdb"
    Just postgresPath <- findExecutable "postgres"

    -- create symlinks
    createSymbolicLink initDbPath $ dir <> "/initdb"
    createSymbolicLink postgresPath $ dir <> "/postgres"

    path <-  getEnv "PATH"

    bracket (setEnv "PATH" dir) (const $ setEnv "PATH" path) $ \_ ->
      action `shouldThrow` isDoesNotExistError

throwsIfInitDbIsNotOnThePath :: IO a -> Spec
throwsIfInitDbIsNotOnThePath action = it "throws if initdb is not on the path" $ do
  path <-  getEnv "PATH"

  bracket (setEnv "PATH" "/foo") (const $ setEnv "PATH" path) $ \_ ->
    action `shouldThrow` isDoesNotExistError

withAnyPlan :: SpecWith Runner
withAnyPlan = do
  it "start/stop the postgres process is running and then it is not" $ withRunner $ \db@DB{..} -> do
    getProcessExitCode (postgresProcessHandle dbPostgresProcess) `shouldReturn` Nothing

    stop db

    getProcessExitCode (postgresProcessHandle dbPostgresProcess) `shouldReturn` Just ExitSuccess

  it "Can connect to the db after it starts" $ withRunner $ \db -> do
    one <- fmap (PG.fromOnly . head) $
      bracket (PG.connectPostgreSQL $ toConnectionString db ) PG.close $
        \conn -> PG.query_ conn "SELECT 1"

    one `shouldBe` (1 :: Int)

  it "cleans up temp files" $ \(Runner runner) -> do
    initialFiles <- listDirectory "/tmp"
    runner $ const $ pure ()
    listDirectory "/tmp" `shouldReturn` initialFiles

-- This assumes that the directory is initially empty
withInitDbEmptyInitially :: SpecWith Runner
withInitDbEmptyInitially = describe "with active initDb non-empty folder initially" $
  it "the data directory has been initialize" $ withRunner $ \DB {..} -> do
    initialFiles <- listDirectory $ toFilePath $ resourcesDataDir $ dbResources
    initialFiles `shouldContain` ["PG_VERSION"]

createDbCreatesTheDb :: String -> SpecWith Runner
createDbCreatesTheDb dbName = describe "createdb " $
  it "creates the db if it didn't exit" $ withRunner $ \db -> do
    result <- bracket (PG.connectPostgreSQL $ toConnectionString db ) PG.close $
      \conn -> fmap (PG.fromOnly . head) $ PG.query_ conn $ fromString $
        "SELECT EXISTS (SELECT datname FROM pg_catalog.pg_database WHERE datname = '" <> dbName <> "')"
    result `shouldBe` True

createDbThrowsIfTheDbExists :: SpecWith Runner
createDbThrowsIfTheDbExists = describe "createdb" $
  it "throws if the db is not there" $ \(Runner runner) ->
    runner (const $ pure ()) `shouldThrow` (== CreateDbFailed (ExitFailure 1))

spec :: Spec
spec = do
  theDefaultResources <- runIO defaultPartialResources
  theStandardProcessOptions <- runIO standardProcessOptions
  describe "start" $ do
    let startAction = bracket (either throwIO pure =<< start) stop (const $ pure ())
    throwsIfInitDbIsNotOnThePath startAction
    throwsIfCreateDbIsNotOnThePath startAction
  describe "startWith" $ do
    let startAction plan = bracket (either throwIO pure =<< startWith plan) stop pure
    throwsIfInitDbIsNotOnThePath $ startAction theDefaultResources
    throwsIfCreateDbIsNotOnThePath $ startAction theDefaultResources
    invalidOptionsFailsQuickly $ void . startAction
    customOptionsWork $ \plan f ->
      bracket (either throwIO pure =<< startWith plan) stop f
  describe "with" $ do
    let startAction = either throwIO pure =<< with (const $ pure ())
    throwsIfInitDbIsNotOnThePath startAction
    throwsIfCreateDbIsNotOnThePath startAction
  describe "withPlan" $ do
    let startAction plan = either throwIO pure =<<
          withPlan plan pure

    throwsIfInitDbIsNotOnThePath $ startAction theDefaultResources
    throwsIfCreateDbIsNotOnThePath $ startAction theDefaultResources
    invalidOptionsFailsQuickly $ void . startAction
    customOptionsWork $ \plan f -> either throwIO pure =<<
      withPlan plan f

  let someStandardTests dbName= do
        withAnyPlan
        withInitDbEmptyInitially
        createDbCreatesTheDb dbName

  describe "start/stop" $ do
    before (pure $ Runner $ \f -> bracket (either throwIO pure =<< start) stop f) $ do
      someStandardTests "test"
      defaultOptionsShouldMatchDefaultPlan

    let invalidCreateDbPlan = updateCreateDb theDefaultResources $
          Mappend $ pure $ theStandardProcessOptions
            { partialProcessOptionsCmdLine = Mappend ["template1"]
            }
    before (pure $ Runner $ \f -> bracket (either throwIO pure =<< startWith invalidCreateDbPlan) stop f) $
      createDbThrowsIfTheDbExists


    let noCreateTemplate1 = mempty
          { partialResourcesPlan = mempty
              { partialPlanCreateDb = Replace Nothing
              , partialPlanPostgres = mempty
                  { partialPostgresPlanClientOptions = mempty
                    { Client.dbname = pure "template1"
                    }
                  }
              }
          }
        noCreateDbPlan = theDefaultResources <> noCreateTemplate1
    before (pure $ Runner $ \f -> bracket (either throwIO pure =<< startWith noCreateDbPlan) stop f) $
      someStandardTests "template1"

    before (createTempDirectory "/tmp" "tmp-postgres-test") $ after rmDirIgnoreErrors $ do
      it "fails on non-empty data directory" $ \dirPath -> do
        writeFile (dirPath <> "/PG_VERSION") "1 million"
        let nonEmptyFolderPlan = theDefaultResources
              { partialResourcesDataDir = Perm dirPath
              }
            startAction = bracket (either throwIO pure =<< startWith nonEmptyFolderPlan) stop $ const $ pure ()

        startAction `shouldThrow` (== InitDbFailed (ExitFailure 1))

      it "works if on non-empty if initdb is disabled" $ \dirPath -> do
        throwIfNotSuccess id =<< system ("initdb " <> dirPath)
        let nonEmptyFolderPlan = theDefaultResources
              { partialResourcesDataDir = Perm dirPath
              , partialResourcesPlan = (partialResourcesPlan theDefaultResources)
                  { partialPlanInitDb = Replace Nothing
                  }
              }
        bracket (either throwIO pure =<< startWith nonEmptyFolderPlan) stop $ \db -> do
          one <- fmap (PG.fromOnly . head) $
            bracket (PG.connectPostgreSQL $ toConnectionString db ) PG.close $
              \conn -> PG.query_ conn "SELECT 1"

          one `shouldBe` (1 :: Int)




{-
data Except = Except
  deriving (Show, Eq, Typeable)

instance Exception Except

countPostgresProcesses :: IO Int
countPostgresProcesses = do
  (exitCode, xs, _) <-  readProcessWithExitCode "pgrep" ["postgres"] []

  unless (exitCode == ExitSuccess || exitCode == ExitFailure 1) $ throwIO exitCode

  pure $ length $ lines xs
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