import Test.Hspec
import Database.Postgres.Temp.Internal.Core
import Database.Postgres.Temp.Internal.Partial
import Database.Postgres.Temp.Internal
import Control.Exception
import System.IO.Error
import System.Process
import System.Exit
import qualified Database.PostgreSQL.Simple as PG
import System.Environment
import System.Posix.Files
import System.IO.Temp
import System.Directory
import qualified Database.PostgreSQL.Simple.Options as Client
import Data.String
import System.Timeout(timeout)
import Control.Monad (void, (<=<))
import Data.Function (fix)
import Control.Concurrent
import Data.Monoid
import Network.Socket.Free (getFreePort)
import qualified Data.Map.Strict as Map

main :: IO ()
main = hspec spec

-- check coverage
-- Test using an existing domain socket

-- Cleanup

fromCreateDb :: Maybe PartialProcessConfig -> Config
fromCreateDb createDb = mempty
  { configPlan = mempty
      { partialPlanCreateDb = createDb
      }
  }

newtype Runner =  Runner (forall a. (DB -> IO a) -> IO a)

withRunner :: (DB -> IO ()) -> Runner -> IO ()
withRunner g (Runner f) = f g

defaultConfigShouldMatchDefaultPlan :: SpecWith Runner
defaultConfigShouldMatchDefaultPlan =
  it "default options should match default plan" $ withRunner $ \DB{..} -> do
    let Resources {..} = dbResources
        Plan {..} = resourcesPlan
        PostgresPlan {..} = planPostgres
    Client.dbname postgresPlanClientConfig `shouldBe` pure "postgres"
    let Temporary tmpDataDir = resourcesDataDir
    tmpDataDir `shouldStartWith` "/tmp/tmp-postgres-data"
    let Just port = getLast $ Client.port postgresPlanClientConfig
    port `shouldSatisfy` (>32768)
    let UnixSocket (Temporary unixSocket) = resourcesSocket
    unixSocket `shouldStartWith` "/tmp/tmp-postgres-socket"
    postgresPlanClientConfig `shouldBe`
      (mempty
        { Client.port = Client.port postgresPlanClientConfig
        , Client.host = Client.host postgresPlanClientConfig
        , Client.dbname = Client.dbname postgresPlanClientConfig
        }
      )

customConfigWork :: (Config -> (DB -> IO ()) -> IO ()) -> Spec
customConfigWork action = do
  let expectedDbName = "thedb"
      expectedPassword = "password"
      expectedUser = "user-name"
      expectedDuration = "100ms"
      extraConfig = "log_min_duration_statement='" <> expectedDuration <> "'"

  it "returns the right client options for the plan" $ do
    let customPlan = mempty
          { configPlan = mempty
              { partialPlanPostgres = mempty
                  { partialPostgresPlanClientConfig = mempty
                      { Client.user     = pure expectedUser
                      , Client.password = pure expectedPassword
                      , Client.dbname   = pure expectedDbName
                      }
                  }
              , partialPlanInitDb = pure standardProcessConfig
                  { partialProcessConfigCmdLine = mempty
                      { partialCommandLineArgsKeyBased =
                          Map.singleton "--username=" $ Just "user-name"
                      }
                  , partialProcessConfigEnvVars = mempty
                      { partialEnvVarsSpecific = Map.singleton "PGPASSWORD" "password"
                      , partialEnvVarsInherit = pure True
                      }
                  }
              , partialPlanCreateDb = pure standardProcessConfig
                { partialProcessConfigCmdLine = mempty
                  { partialCommandLineArgsKeyBased =
                      Map.singleton "--username=" $ Just "user-name"
                  , partialCommandLineArgsIndexBased =
                      Map.singleton 0 expectedDbName
                  }
                , partialProcessConfigEnvVars =
                    mempty
                      { partialEnvVarsSpecific = Map.singleton "PGPASSWORD" "password"
                      , partialEnvVarsInherit = pure True
                      }
                }
              , partialPlanConfig = [extraConfig]
              }
          }
    -- hmm maybe I should provide lenses
    let combinedResources = defaultConfig <> customPlan

    action combinedResources $ \db@DB {..} -> do
      bracket (PG.connectPostgreSQL $ toConnectionString db) PG.close $ \conn -> do
        [PG.Only actualDuration] <- PG.query_ conn "SHOW log_min_duration_statement"
        actualDuration `shouldBe` expectedDuration

      let Resources {..} = dbResources
          Plan {..} = resourcesPlan
          actualOptions = postgresPlanClientConfig planPostgres
          actualPostgresConfig = planConfig
      Client.user actualOptions `shouldBe` pure expectedUser
      Client.dbname actualOptions `shouldBe` pure expectedDbName
      Client.password actualOptions `shouldBe` pure expectedPassword
      lines actualPostgresConfig `shouldContain` defaultPostgresConfig <> [extraConfig]

invalidConfigFailsQuickly :: (Config -> IO ()) -> Spec
invalidConfigFailsQuickly action = it "quickly fails with an invalid option" $ do
  let customPlan = mempty
        { configPlan = mempty
            { partialPlanConfig =
                [ "log_directory = /this/does/not/exist"
                , "logging_collector = true"
                ]
            }
        }
  timeout 5000000 (action $ defaultConfig <> customPlan) `shouldThrow`
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
  let defaultIpPlan = defaultConfig
        { configSocket = PIpSocket $ Last Nothing
        }

      specificHostIpPlan = defaultConfig
        { configSocket = PIpSocket $ pure "localhost"
        }

  describe "start" $ do
    let startAction = bracket (either throwIO pure =<< start) stop (const $ pure ())
    throwsIfInitDbIsNotOnThePath startAction
  describe "startConfig" $ do
    let startAction plan = bracket (either throwIO pure =<< startConfig plan) stop pure
    throwsIfInitDbIsNotOnThePath $ startAction defaultConfig
    invalidConfigFailsQuickly $ void . startAction
    customConfigWork $ \plan@Config{..} f ->
      bracket (either throwIO pure =<< startConfig plan) stop f
  describe "with" $ do
    let startAction = either throwIO pure =<< with (const $ pure ())
    throwsIfInitDbIsNotOnThePath startAction
  describe "withConfig" $ do
    let startAction plan = either throwIO pure =<<
          withConfig plan pure

    throwsIfInitDbIsNotOnThePath $ startAction defaultConfig
--    throwsIfCreateDbIsNotOnThePath $ startAction defaultConfig
    invalidConfigFailsQuickly $ void . startAction
    customConfigWork $ \plan f -> either throwIO pure =<<
      withConfig plan f

  let someStandardTests dbName= do
        withAnyPlan
        withInitDbEmptyInitially
        createDbCreatesTheDb dbName

  describe "restart" $ do
    let startAction f =
          bracket (either throwIO pure
            =<< restart
            =<< either throwIO pure
            =<< start) stop f
    before (pure $ Runner startAction) $ do
      someStandardTests "postgres"
      defaultConfigShouldMatchDefaultPlan

  describe "withRestart" $ do
    let startAction f = bracket (either throwIO pure =<< start) stop $ \db ->
          either throwIO pure =<< withRestart db f
    before (pure $ Runner startAction) $ do
      someStandardTests "postgres"
      defaultConfigShouldMatchDefaultPlan

  describe "start/stop" $ do
    before (pure $ Runner $ \f -> bracket (either throwIO pure =<< start) stop f) $ do
      someStandardTests "postgres"
      defaultConfigShouldMatchDefaultPlan
      it "stopPostgres cannot be connected to" $ withRunner $ \db -> do
        stopPostgres db `shouldReturn` ExitSuccess
        PG.connectPostgreSQL (toConnectionString db) `shouldThrow`
          (\(_ :: IOError) -> True)

      it "reloadConfig works" $ withRunner $ \db@DB{..} -> do
        let dataDir = toFilePath (resourcesDataDir dbResources)
            expectedDuration = "100ms"
            extraConfig = "log_min_duration_statement='" <> expectedDuration <> "'"
        appendFile (dataDir ++ "/postgresql.conf") $ extraConfig

        reloadConfig db

        bracket (PG.connectPostgreSQL $ toConnectionString db) PG.close $ \conn -> do
          [PG.Only actualDuration] <- PG.query_ conn "SHOW log_min_duration_statement"
          actualDuration `shouldBe` expectedDuration

    let invalidCreateDbPlan = defaultConfig <> fromCreateDb
          ( pure $ standardProcessConfig
              { partialProcessConfigCmdLine = mempty
                { partialCommandLineArgsIndexBased =
                    Map.singleton 0 "template1"
                }
              }
          )
    before (pure $ Runner $ \f -> bracket (either throwIO pure =<< startConfig invalidCreateDbPlan) stop f) $
      createDbThrowsIfTheDbExists

    let noCreateTemplate1 = mempty
          { configPlan = mempty
              { partialPlanCreateDb = Nothing
              , partialPlanPostgres = mempty
                  { partialPostgresPlanClientConfig = mempty
                    { Client.dbname = pure "template1"
                    }
                  }
              }
          }
        noCreateDbPlan = defaultConfig <> noCreateTemplate1
    before (pure $ Runner $ \f -> bracket (either throwIO pure =<< startConfig noCreateDbPlan) stop f) $
      someStandardTests "template1"

    before (pure $ Runner $ \f -> bracket (either throwIO pure =<< startConfig defaultIpPlan) stop f) $
      someStandardTests "postgres"

    before (pure $ Runner $ \f -> bracket (either throwIO pure =<< startConfig specificHostIpPlan) stop f) $
      someStandardTests "postgres"

    thePort <- runIO getFreePort
    let planFromCustomUserDbConnection = optionsToDefaultConfig mempty
          { Client.dbname = pure "fancy"
          , Client.user   = pure "some_user"
          , Client.port   = pure thePort
          }

    before (pure $ Runner $ \f -> bracket (either throwIO pure =<< startConfig planFromCustomUserDbConnection) stop f) $
      someStandardTests "fancy"

    before (createTempDirectory "/tmp" "tmp-postgres-test") $ after rmDirIgnoreErrors $ do
      it "fails on non-empty data directory" $ \dirPath -> do
        writeFile (dirPath <> "/PG_VERSION") "1 million"
        let nonEmptyFolderPlan = defaultConfig
              { configDataDir = PPermanent dirPath
              }
            startAction = bracket (either throwIO pure =<< startConfig nonEmptyFolderPlan) stop $ const $ pure ()

        startAction `shouldThrow` (== InitDbFailed (ExitFailure 1))

      it "works if on non-empty if initdb is disabled" $ \dirPath -> do
        throwIfNotSuccess id =<< system ("initdb " <> dirPath)
        let nonEmptyFolderPlan = defaultConfig
              { configDataDir = PPermanent dirPath
              , configPlan = (configPlan defaultConfig)
                  { partialPlanInitDb = Nothing
                  }
              }
        bracket (either throwIO pure =<< startConfig nonEmptyFolderPlan) stop $ \db -> do
          one <- fmap (PG.fromOnly . head) $
            bracket (PG.connectPostgreSQL $ toConnectionString db ) PG.close $
              \conn -> PG.query_ conn "SELECT 1"

          one `shouldBe` (1 :: Int)

    let justBackupResources = mempty
          { configPlan = mempty
              { partialPlanConfig =
                  [ "wal_level=replica"
                  , "archive_mode=on"
                  , "max_wal_senders=2"
                  , "fsync=on"
                  , "synchronous_commit=on"
                  ]
              }
          }
        backupResources = defaultConfig <> justBackupResources
    before (pure $ Runner $ \f -> bracket (either throwIO pure =<< startConfig backupResources) stop f) $
      it "can support backup and restore" $ withRunner $ \db@DB {..} -> do
        let dataDir = toFilePath (resourcesDataDir dbResources)
        appendFile (dataDir ++ "/pg_hba.conf") $ "local replication all trust"
        withTempDirectory "/tmp" "tmp-postgres-backup" $ \tempDir -> do
          let walArchiveDir = tempDir ++ "/archive"
              baseBackupFile = tempDir ++ "/backup"
              archiveLine = "archive_command = " ++
                "'test ! -f " ++ walArchiveDir ++ "/%f && cp %p " ++ walArchiveDir ++ "/%f'\n"
          appendFile (dataDir ++ "/postgresql.conf") $ archiveLine

          createDirectory walArchiveDir

          reloadConfig db

          let Just port = getLast $ Client.port $ postgresProcessClientConfig dbPostgresProcess
              Just host = getLast $ Client.host $ postgresProcessClientConfig dbPostgresProcess
              backupCommand = "pg_basebackup -D " ++ baseBackupFile ++ " --format=tar -p"
                ++ show port ++ " -h" ++ host

          system backupCommand `shouldReturn` ExitSuccess

          bracket (PG.connectPostgreSQL $ toConnectionString db ) PG.close $ \conn -> do
            _ <- PG.execute_ conn "CREATE TABLE foo(id int PRIMARY KEY);"
            _ <- PG.execute_ conn "BEGIN ISOLATION LEVEL READ COMMITTED READ WRITE; INSERT INTO foo (id) VALUES (1); COMMIT"
            _ :: [PG.Only String] <- PG.query_ conn "SELECT pg_walfile_name(pg_switch_wal())"
            _ :: [PG.Only String] <- PG.query_ conn "SELECT pg_walfile_name(pg_create_restore_point('pitr'))"
            _ <- PG.execute_ conn "BEGIN ISOLATION LEVEL READ COMMITTED READ WRITE; INSERT INTO foo (id) VALUES (2); COMMIT"

            PG.query_ conn "SELECT id FROM foo ORDER BY id ASC"
              `shouldReturn` [PG.Only (1 :: Int), PG.Only 2]

          stopPostgres db `shouldReturn` ExitSuccess

          removeDirectoryRecursive dataDir
          createDirectory dataDir

          let untarCommand = "tar -C" ++ dataDir ++ " -xf " ++ baseBackupFile ++ "/base.tar"
          system untarCommand `shouldReturn` ExitSuccess

          system ("chmod -R 700 " ++ dataDir) `shouldReturn` ExitSuccess

          writeFile (dataDir ++ "/recovery.conf") $ "recovery_target_name='pitr'\nrecovery_target_action='promote'\nrecovery_target_inclusive=true\nrestore_command='"
             ++ "cp " ++ walArchiveDir ++ "/%f %p'"

          either throwIO pure <=< withRestart db $ \newDb -> do
            bracket (PG.connectPostgreSQL $ toConnectionString newDb) PG.close $ \conn -> do
              fix $ \next -> do
                fmap (PG.fromOnly . head) (PG.query_ conn "SELECT pg_is_in_recovery()") >>= \case
                  True -> threadDelay 100000 >> next
                  False -> pure ()

              PG.query_ conn "SELECT id FROM foo ORDER BY id ASC"
                `shouldReturn` [PG.Only (1 :: Int)]
