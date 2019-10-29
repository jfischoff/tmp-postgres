module Database.Postgres.Temp.Core where
import qualified Database.PostgreSQL.Simple.Options as PostgresClient
import qualified Database.PostgreSQL.Simple as PG
import System.Process.Internals
import System.Exit (ExitCode(..))
import Data.String
import System.Posix.Signals (sigINT, signalProcess)
import Control.Exception
import System.Process (getProcessExitCode, waitForProcess)
import Data.Foldable (for_)
import Control.Concurrent.Async (race_)
import Control.Monad (forever, (>=>))
import Control.Concurrent (threadDelay)
import Data.Typeable
import System.IO
import System.Process
-- TODO use pg_ctr if the exe's are not found but pg_ctr is
-- TOOD move the wait for postgres connection in here
data Event
  = StartPostgres
  | WaitForDB
  deriving (Show, Eq, Ord)

data StartError
  = StartPostgresFailed ExitCode
  | InitDbFailed ExitCode
  | CreateDbFailed ExitCode
  | CompletePlanFailed [String] -- TODO move elsewhere
  deriving (Show, Eq, Ord, Typeable)

instance Exception StartError

type Logger = Event -> IO ()

waitForDB :: PostgresClient.Options -> IO ()
waitForDB options = do
  let theConnectionString = PostgresClient.toConnectionString options
  try (bracket (PG.connectPostgreSQL theConnectionString) PG.close mempty) >>= \case
    Left (_ :: IOError) -> threadDelay 10000 >> waitForDB options
    Right () -> return ()

-------------------------------------------------------------------------------
-- ProcessOptions
-------------------------------------------------------------------------------
-- remove name
data ProcessOptions = ProcessOptions
  { processOptionsEnvVars :: [(String, String)]
  , processOptionsCmdLine :: [String]
  , processOptionsStdIn   :: Handle
  , processOptionsStdOut  :: Handle
  , processOptionsStdErr  :: Handle
  }
-------------------------------------------------------------------------------
-- Starting Processes
-------------------------------------------------------------------------------
fourth :: (a, b, c, d) -> d
fourth (_, _, _, x) = x

evaluateProcess :: String -> ProcessOptions -> IO ProcessHandle
evaluateProcess name ProcessOptions {..} = fmap fourth $
  createProcess_ name $
    (proc name processOptionsCmdLine)
      { std_err = UseHandle processOptionsStdErr
      , std_out = UseHandle processOptionsStdOut
      , std_in  = UseHandle processOptionsStdIn
      , env     = Just processOptionsEnvVars
      }

executeProcess :: String -> ProcessOptions -> IO ExitCode
executeProcess name = evaluateProcess name >=> waitForProcess
-------------------------------------------------------------------------------
-- PostgresPlan
-------------------------------------------------------------------------------
data PostgresPlan = PostgresPlan
  { postgresPlanProcessOptions :: ProcessOptions
  , postgresPlanClientOptions  :: PostgresClient.Options
  }
-------------------------------------------------------------------------------
-- PostgresProcess
-------------------------------------------------------------------------------
data PostgresProcess = PostgresProcess
  { postgresProcessClientOptions :: PostgresClient.Options
  , postgresProcessHandle :: ProcessHandle
  }
-------------------------------------------------------------------------------
-- PostgresProcess Life cycle management
-------------------------------------------------------------------------------
-- | Force all connections to the database to close. Can be useful in some
--   testing situations.
--   Called during shutdown as well.
terminateConnections :: PostgresClient.Options-> IO ()
terminateConnections options = do
  let theConnectionString = PostgresClient.toConnectionString options
      terminationQuery = fromString $ unlines
        [ "SELECT pg_terminate_backend(pid)"
        , "FROM pg_stat_activity"
        , "WHERE datname=?;"
        ]
  e <- try $ bracket (PG.connectPostgreSQL theConnectionString) PG.close $
    \conn -> PG.execute conn terminationQuery
      [PostgresClient.oDbname options]
  case e of
    Left (_ :: IOError) -> pure ()
    Right _ -> pure ()

-- | Stop the postgres process. This function attempts to the 'pidLock' before running.
--   'stopPostgres' will terminate all connections before shutting down postgres.
--   'stopPostgres' is useful for testing backup strategies.
stopPostgresProcess :: PostgresProcess -> IO ExitCode
stopPostgresProcess PostgresProcess{..} = do
  withProcessHandle postgresProcessHandle $ \case
    OpenHandle p   -> do
      -- try to terminate the connects first. If we can't terminate still
      -- keep shutting down
      terminateConnections postgresProcessClientOptions

      signalProcess sigINT p
    OpenExtHandle {} -> pure () -- TODO log windows is not supported
    ClosedHandle _ -> return ()

  exitCode <- waitForProcess postgresProcessHandle
  pure exitCode

startPostgres :: Logger -> PostgresPlan -> IO PostgresProcess
startPostgres logger PostgresPlan {..} = do
  logger StartPostgres
  -- Start postgres and stop if a exception occurs when
  -- trying to connection
  -- Keep checking if the postgres process has exited.
  -- Break the connection waiting loop if it has exited.
  bracketOnError (PostgresProcess postgresPlanClientOptions <$> evaluateProcess "postgres" postgresPlanProcessOptions)
    stopPostgresProcess $ \result@PostgresProcess {..} -> do
      let checkForCrash = do
            mExitCode <- getProcessExitCode postgresProcessHandle
            for_ mExitCode (throwIO . StartPostgresFailed)

      logger WaitForDB
      let options = postgresPlanClientOptions
            { PostgresClient.oDbname = "template1"
            }

      waitForDB options
        `race_` forever (checkForCrash >> threadDelay 100000)

      return result

-- extend with a version of startWith that takes in a Plan. Change the of Plan to PartialPlan
-- refactor start with to create plan and this call this start with.
-------------------------------------------------------------------------------
-- Plan
-------------------------------------------------------------------------------
data Plan = Plan
  { planLogger        :: Logger
  , planInitDb        :: Maybe ProcessOptions
  , planCreateDb      :: Maybe ProcessOptions
  , planPostgres      :: PostgresPlan
  , planConfig        :: String
  , planDataDirectory :: FilePath
  }

throwIfNotSuccess :: Exception e => (ExitCode -> e) -> ExitCode -> IO ()
throwIfNotSuccess f = \case
  ExitSuccess -> pure ()
  e -> throwIO $ f e
-- Start optional calls initdb, optionally calls createdb, and
-- unconditionally calls postgres
startPlan :: Plan -> IO PostgresProcess
startPlan Plan {..} = do
  for_ planInitDb  $ executeProcess "initdb" >=>
    throwIfNotSuccess InitDbFailed

  writeFile (planDataDirectory <> "/postgresql.conf") planConfig

  bracketOnError (startPostgres planLogger planPostgres) stopPostgresProcess $ \result -> do
    for_ planCreateDb $  executeProcess "createdb" >=>
      throwIfNotSuccess CreateDbFailed

    pure result
