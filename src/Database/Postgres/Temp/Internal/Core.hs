{-|
This module provides the low level functionality for running @initdb@, @postgres@ and @createdb@ to make a database.

See 'initPlan' for more details.
|-}
module Database.Postgres.Temp.Internal.Core where
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

-- | Internal events for debugging
data Event
  = StartPostgres
  | WaitForDB
  deriving (Show, Eq, Ord)

-- | A list of failures that can occur when starting. This is not
--   and exhaustive list but covers the errors that the system
--   catches for the user.
data StartError
  = StartPostgresFailed ExitCode
  -- ^ @postgres@ failed before a connection succeeded. Most likely this
  --   is due to invalid configuration
  | InitDbFailed ExitCode
  -- ^ @initdb@ failed. This can be from invalid configuration or using a
  --   non-empty data directory
  | CreateDbFailed ExitCode
  -- ^ @createdb@ failed. This can be from invalid configuration or
  --   the database might already exist.
  | CompletePlanFailed [String]
  -- ^ The 'Database.Postgres.Temp.Partial.PartialPlan' was missing info and a complete 'Plan' could
  --   not be created.
  deriving (Show, Eq, Ord, Typeable)

instance Exception StartError

-- | A way to log internal 'Event's
type Logger = Event -> IO ()

-- TODO. Add a Retrying Event
-- | @postgres@ is not ready until we are able to successfully connect.
--   'waitForDB' attempts to connect over and over again and returns
--   after the first successful connection.
waitForDB :: PostgresClient.Options -> IO ()
waitForDB options = do
  let theConnectionString = PostgresClient.toConnectionString options
      startAction = PG.connectPostgreSQL theConnectionString
  try (bracket startAction PG.close mempty) >>= \case
    Left (_ :: IOError) -> threadDelay 10000 >> waitForDB options
    Right () -> return ()

-- | 'ProcessConfig' contains the configuration necessary for starting a
--   process. It is essentially a stripped down 'System.Process.CreateProcess'.
data ProcessConfig = ProcessConfig
  { processConfigEnvVars :: [(String, String)]
  -- ^ Environment variables
  , processConfigCmdLine :: [String]
  -- ^ Command line arguements
  , processConfigStdIn   :: Handle
  -- ^ The 'Handle' for standard input
  , processConfigStdOut  :: Handle
  -- ^ The 'Handle' for standard output
  , processConfigStdErr  :: Handle
  -- ^ The 'Handle' for standard error
  }

-- | Start a process interactively and return the 'ProcessHandle'
startProcess
  :: String
  -- ^ Process name
  -> ProcessConfig
  -- ^ Process config
  -> IO ProcessHandle
startProcess name ProcessConfig {..} = (\(_, _, _, x) -> x) <$>
  createProcess_ name (proc name processConfigCmdLine)
    { std_err = UseHandle processConfigStdErr
    , std_out = UseHandle processConfigStdOut
    , std_in  = UseHandle processConfigStdIn
    , env     = Just processConfigEnvVars
    }

-- | Start a process and block until it finishes return the 'ExitCode'.
executeProcess
  :: String
  -- ^ Process name
  -> ProcessConfig
  -- ^ Process config
  -> IO ExitCode
executeProcess name = startProcess name >=> waitForProcess
-------------------------------------------------------------------------------
-- PostgresProcess Life cycle management
-------------------------------------------------------------------------------
-- | PostgresPlan is used be 'startPostgresProcess' to start the @postgres@
--   and then attempt to connect to it.
data PostgresPlan = PostgresPlan
  { postgresPlanProcessConfig :: ProcessConfig
  -- ^ The process config for @postgres@
  , postgresPlanClientConfig  :: PostgresClient.Options
  -- ^ Connection options. Used to verify that @postgres@ is ready.
  }

-- | The output of calling 'startPostgresProcess'.
data PostgresProcess = PostgresProcess
  { postgresProcessClientConfig :: PostgresClient.Options
  -- ^ Connection options
  , postgresProcessHandle :: ProcessHandle
  -- ^ @postgres@ process handle
  }

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

-- | Stop the @postgres@ process after attempting to terminate all the
--   connections.
stopPostgresProcess :: PostgresProcess -> IO ExitCode
stopPostgresProcess PostgresProcess{..} = do
  withProcessHandle postgresProcessHandle $ \case
    OpenHandle p   -> do
      -- try to terminate the connects first. If we can't terminate still
      -- keep shutting down
      terminateConnections postgresProcessClientConfig

      signalProcess sigINT p
    OpenExtHandle {} -> pure () -- TODO log windows is not supported
    ClosedHandle _ -> return ()

  exitCode <- waitForProcess postgresProcessHandle
  pure exitCode

-- | Start the @postgres@ process and block until a successful connection
--   occurs. A separate thread we continously check to see if the @postgres@
--   process has
startPostgresProcess :: Logger -> PostgresPlan -> IO PostgresProcess
startPostgresProcess logger PostgresPlan {..} = do
  logger StartPostgres

  let startAction = PostgresProcess postgresPlanClientConfig
        <$> startProcess "postgres" postgresPlanProcessConfig

  -- Start postgres and stop if an exception occurs
  bracketOnError startAction stopPostgresProcess $
    \result@PostgresProcess {..} -> do
      -- A helper to check if the process has died
      let checkForCrash = do
            mExitCode <- getProcessExitCode postgresProcessHandle
            for_ mExitCode (throwIO . StartPostgresFailed)

      logger WaitForDB
      -- We assume that 'template1' exist and make connection
      -- options to test if postgres is ready.
      let options = postgresPlanClientConfig
            { PostgresClient.oDbname = "template1"
            }

      -- Block until a connection succeeds or postgres crashes
      waitForDB options
        `race_` forever (checkForCrash >> threadDelay 100000)

      -- Postgres is now ready so return
      return result
-------------------------------------------------------------------------------
-- Plan
-------------------------------------------------------------------------------
-- | 'Plan' is the low level configuration necessary for creating a database
--   starting @postgres@ and creating a database. There is no validation done
--   on the 'Plan'. It is recommend that one use the higher level functions
--   such as 'Database.Postgres.Temp.start' which will generate plans that
--   are valid. 'Plan's are used internally but are exposed if the higher
--   level plan generation is not sufficent.
data Plan = Plan
  { planLogger        :: Logger
  , planInitDb        :: Maybe ProcessConfig
  , planCreateDb      :: Maybe ProcessConfig
  , planPostgres      :: PostgresPlan
  , planConfig        :: String
  , planDataDirectory :: FilePath
  }

-- | A simple helper to throw 'ExitCode's when they are 'ExitFailure'.
throwIfNotSuccess :: Exception e => (ExitCode -> e) -> ExitCode -> IO ()
throwIfNotSuccess f = \case
  ExitSuccess -> pure ()
  e -> throwIO $ f e

-- | 'initPlan' optionally calls @initdb@, optionally calls @createdb@ and
--   unconditionally calls @postgres@.
--   Additionally it writes a \"postgresql.conf\" and does not return until
--   the @postgres@ process is ready. See 'startPostgresProcess' for more
--   details.
initPlan :: Plan -> IO PostgresProcess
initPlan Plan {..} = do
  for_ planInitDb  $ executeProcess "initdb" >=>
    throwIfNotSuccess InitDbFailed

  -- We must provide a config file before we can start postgres.
  writeFile (planDataDirectory <> "/postgresql.conf") planConfig

  let startAction = startPostgresProcess planLogger planPostgres

  bracketOnError startAction stopPostgresProcess $ \result -> do
    for_ planCreateDb $  executeProcess "createdb" >=>
      throwIfNotSuccess CreateDbFailed

    pure result

-- | Stop the @postgres@ process. See 'stopPostgresProcess' for more details.
shutdownPlan :: PostgresProcess -> IO ExitCode
shutdownPlan = stopPostgresProcess