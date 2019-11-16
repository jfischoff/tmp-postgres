import Test.Hspec
import Database.Postgres.Temp.Internal.Core
import Database.Postgres.Temp.Internal.Config
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

withConfig' :: Config -> (DB -> IO a) -> IO a
withConfig' config = either throwIO pure <=< withConfig config

with' :: (DB -> IO a) -> IO a
with' = either throwIO pure <=< with

fromCreateDb :: Maybe ProcessConfig -> Config
fromCreateDb createDb = mempty
  { plan = mempty
      { createDbConfig = createDb
      }
  }

newtype Runner =  Runner (forall a. (DB -> IO a) -> IO a)

withRunner :: (DB -> IO ()) -> Runner -> IO ()
withRunner g (Runner f) = f g

--
-- TODO this test should only use the public interface
--

defaultConfigShouldMatchDefaultPlan :: SpecWith Runner
defaultConfigShouldMatchDefaultPlan =
  it "default options should match default plan" $ withRunner $ \DB{..} -> do
    let Resources {..} = dbResources
        CompletePlan {..} = resourcesPlan
        CompletePostgresPlan {..} = completePlanPostgres
    Client.dbname completePostgresPlanClientOptions `shouldBe` pure "postgres"
    let CTemporary tmpDataDir = resourcesDataDir
    tmpDataDir `shouldStartWith` "/tmp/tmp-postgres-data"
    let Just port = getLast $ Client.port completePostgresPlanClientOptions
    port `shouldSatisfy` (>32768)
    let CUnixSocket (CTemporary unixSocket) = resourcesSocket
    unixSocket `shouldStartWith` "/tmp/tmp-postgres-socket"
    completePostgresPlanClientOptions `shouldBe`
      (mempty
        { Client.port = Client.port completePostgresPlanClientOptions
        , Client.host = Client.host completePostgresPlanClientOptions
        , Client.dbname = Client.dbname completePostgresPlanClientOptions
        }
      )


customConfigWork :: (Config -> (DB -> IO ()) -> IO ()) -> Spec
customConfigWork action = do
  let expectedDbName = "thedb"
      expectedPassword = "password"
      expectedUser = "user-name"
      expectedDuration = "100ms"
      extraConfig = "log_min_duration_statement='" <> expectedDuration <> "'"

--
-- This is basically the optionsToDefaultConfig but also tests custom postgresql.conf values
--

  it "returns the right client options for the plan" $ withTempDirectory "/tmp" "tmp-postgres-spec" $ \tmpDir -> do
    let customPlan = mempty
          { temporaryDirectory = pure tmpDir
          , plan = mempty
              { postgresPlan = mempty
                  { connectionOptions = mempty
                      { Client.user     = pure expectedUser
                      , Client.password = pure expectedPassword
                      , Client.dbname   = pure expectedDbName
                      }
                  }
              , initDbConfig = pure silentProcessConfig
                  { commandLine = mempty
                      { keyBased =
                          Map.singleton "--username=" $ Just "user-name"
                      }
                  , environmentVariables = mempty
                      { specific = Map.singleton "PGPASSWORD" "password"
                      , inherit = pure True
                      }
                  }
              , createDbConfig = pure silentProcessConfig
                { commandLine = mempty
                  { keyBased =
                      Map.singleton "--username=" $ Just "user-name"
                  , indexBased =
                      Map.singleton 0 expectedDbName
                  }
                , environmentVariables =
                    mempty
                      { specific = Map.singleton "PGPASSWORD" "password"
                      , inherit = pure True
                      }
                }
              , postgresConfigFile = [extraConfig]
              }
          }
    -- hmm maybe I should provide lenses
    let combinedResources = silentConfig <> customPlan

    initialFiles <- listDirectory tmpDir

    action combinedResources $ \db@DB {..} -> do
      bracket (PG.connectPostgreSQL $ toConnectionString db) PG.close $ \conn -> do
        [PG.Only actualDuration] <- PG.query_ conn "SHOW log_min_duration_statement"
        actualDuration `shouldBe` expectedDuration

      let Resources {..} = dbResources
          CompletePlan {..} = resourcesPlan
          actualOptions = completePostgresPlanClientOptions completePlanPostgres
          actualPostgresConfig = completePlanConfig
      Client.user actualOptions `shouldBe` pure expectedUser
      Client.dbname actualOptions `shouldBe` pure expectedDbName
      Client.password actualOptions `shouldBe` pure expectedPassword
      lines actualPostgresConfig `shouldContain` defaultPostgresConfig <> [extraConfig]

    listDirectory tmpDir `shouldReturn` initialFiles

invalidConfigFailsQuickly :: (Config -> IO ()) -> Spec
invalidConfigFailsQuickly action = it "quickly fails with an invalid option" $ do
  let customPlan = mempty
        { plan = mempty
            { postgresConfigFile =
                [ "log_directory = /this/does/not/exist"
                , "logging_collector = true"
                ]
            }
        }
  timeout 5000000 (action $ silentConfig <> customPlan) `shouldThrow`
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
    runner (const $ pure ()) `shouldThrow` (\(CreateDbFailed theOut theErr code) -> do
          code == ExitFailure 1 && length theErr > 0 && theOut == "")

spec :: Spec
spec = do
  let defaultIpPlan = silentConfig
        { socketClass = IpSocket $ Last Nothing
        }

      specificHostIpPlan = silentConfig
        { socketClass = IpSocket $ pure "localhost"
        }

  describe "withConfig" $ do
    let startAction plan = either throwIO pure =<<
          withConfig plan pure

    throwsIfInitDbIsNotOnThePath $ startAction silentConfig
    invalidConfigFailsQuickly $ void . startAction
    customConfigWork $ \plan f -> either throwIO pure =<<
      withConfig plan f

  let someStandardTests dbName= do
        withAnyPlan
        withInitDbEmptyInitially
        createDbCreatesTheDb dbName

  describe "withRestart" $ do
    let startAction f = with' $ \db ->
          either throwIO pure =<< withRestart db f
    before (pure $ Runner startAction) $ do
      someStandardTests "postgres"
      defaultConfigShouldMatchDefaultPlan

--
-- TODO these can be combined
--

  describe "start/stop" $ do
    before (pure $ Runner with') $ do
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

--
--  Keep this test. Invalid createdb options test
--
    let invalidCreateDbPlan = silentConfig <> fromCreateDb
          ( pure $ silentProcessConfig
              { commandLine = mempty
                { indexBased =
                    Map.singleton 0 "template1"
                }
              }
          )
    before (pure $ Runner $ either throwIO pure <=< withConfig invalidCreateDbPlan) $
      createDbThrowsIfTheDbExists

-- TODO
-- combine this test with another plan
-- tests that the dbName can be specified in the options
--
    let noCreateTemplate1 = mempty
          { plan = mempty
              { createDbConfig = Nothing
              , postgresPlan = mempty
                  { connectionOptions = mempty
                    { Client.dbname = pure "template1"
                    }
                  }
              }
          }
        noCreateDbPlan = defaultConfig <> noCreateTemplate1
    before (pure $ Runner $ withConfig' noCreateDbPlan) $
      someStandardTests "template1"

--
-- Test the default IpSocket plan. Can be combined.
--
    before (pure $ Runner $ withConfig' defaultIpPlan) $
      someStandardTests "postgres"

-- Specific IpSocket plan
-- Can be combined
--
    before (pure $ Runner $ withConfig' specificHostIpPlan) $
      someStandardTests "postgres"

--
-- This test is okay but can be combine with other tests
-- and should use withNewDbConf mempty instead
-- also need to test that I can make a specific database name

    before (pure $ Runner $ withConfig' silentConfig) $
      it "withNewDb works" $ withRunner $ \db -> do
        bracket (PG.connectPostgreSQL $ toConnectionString db ) PG.close $
          \conn -> do
            _ <- PG.execute_ conn "CREATE TABLE foo ( id int );"
            void $ PG.execute_ conn "INSERT INTO foo (id) VALUES (1);"

        void $ withNewDb db $ \newDb -> do
          one <- fmap (PG.fromOnly . head) $
            bracket (PG.connectPostgreSQL $ toConnectionString newDb ) PG.close $
              \conn -> PG.query_ conn "SELECT id FROM foo"

          one `shouldBe` (1 :: Int)

--
-- This is a optionsToDefaultConfig test
-- It is missing the host options of both
-- a socket and ip address based
--

    thePort <- runIO getFreePort
    let planFromCustomUserDbConnection = optionsToDefaultConfig mempty
          { Client.dbname   = pure "fancy"
          , Client.user     = pure "some_user"
          , Client.port     = pure thePort
          , Client.password = pure "password"
          , Client.host     = pure "localhost"
          }

    before (pure $ Runner $ withConfig' planFromCustomUserDbConnection) $
      someStandardTests "fancy"

--
-- end optionsToDefaultConfig test
--

--
-- Keep this test -- small connection timeout works
--
    let immediantlyTimeout = silentConfig <> mempty
          { plan = mempty
              { connectionTimeout = pure 0
              }
          }

    before (pure $ Runner $ withConfig' immediantlyTimeout) $
      it "should timeout" $ \(Runner runner) ->
        runner (const $ pure ()) `shouldThrow` (== ConnectionTimedOut)
--
-- End this test
--

--
-- Keep these tests
--
    before (createTempDirectory "/tmp" "tmp-postgres-test") $ after rmDirIgnoreErrors $ do
      it "fails on non-empty data directory" $ \dirPath -> do
        writeFile (dirPath <> "/PG_VERSION") "1 million"
        let nonEmptyFolderPlan = silentConfig
              { dataDirectory = Permanent dirPath
              }
            startAction = withConfig' nonEmptyFolderPlan $ const $ pure ()

        startAction `shouldThrow` (\(InitDbFailed theOut theErr code) -> do
          code == ExitFailure 1 && length theOut > 0 && length theErr > 0)

      it "works if on non-empty if initdb is disabled" $ \dirPath -> do
        throwIfNotSuccess id =<< system ("initdb " <> dirPath)
        let nonEmptyFolderPlan = silentConfig
              { dataDirectory = Permanent dirPath
              , plan = (plan silentConfig)
                  { initDbConfig = Nothing
                  }
              }
        withConfig' nonEmptyFolderPlan $ \db -> do
          one <- fmap (PG.fromOnly . head) $
            bracket (PG.connectPostgreSQL $ toConnectionString db ) PG.close $
              \conn -> PG.query_ conn "SELECT 1"

          one `shouldBe` (1 :: Int)

--
-- Backup "test"
--

    let justBackupResources = mempty
          { plan = mempty
              { postgresConfigFile =
                  [ "wal_level=replica"
                  , "archive_mode=on"
                  , "max_wal_senders=2"
                  , "fsync=on"
                  , "synchronous_commit=on"
                  ]
              }
          }
        backupResources = silentConfig <> justBackupResources
    before (pure $ Runner $ withConfig' backupResources) $
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

          let Just port = getLast $ Client.port $ postgresProcessClientOptions dbPostgresProcess
              Just host = getLast $ Client.host $ postgresProcessClientOptions dbPostgresProcess
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
