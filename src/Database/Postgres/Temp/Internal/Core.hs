{-# OPTIONS_HADDOCK prune #-}
{-|
This module provides the low level functionality for running @initdb@, @postgres@ and @createdb@ to make a database.

See 'startPlan' for more details.
-}
module Database.Postgres.Temp.Internal.Core where

import           Control.Concurrent (threadDelay)
import           Control.Concurrent.Async (race_)
import           Control.Exception
import           Control.Monad (forever, (>=>))
import qualified Data.ByteString.Char8 as BSC
import           Data.Foldable (for_)
import           Data.Monoid
import           Data.String
import           Data.Typeable
import qualified Database.PostgreSQL.Simple as PG
import qualified Database.PostgreSQL.Simple.Options as Client
import           System.Exit (ExitCode(..))
import           System.IO
import           System.Posix.Signals (sigINT, signalProcess)
import           System.Process
import           System.Process.Internals
import           Text.PrettyPrint.ANSI.Leijen hiding ((<$>))

-- | Internal events for debugging
data Event
  = StartPostgres
  | WaitForDB
  | StartPlan String
  | TryToConnect
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
  | CompletePlanFailed String [String]
  -- ^ The 'Database.Postgres.Temp.Partial.PartialPlan' was missing info and a complete 'CompletePlan' could
  --   not be created.
  deriving (Show, Eq, Ord, Typeable)

instance Exception StartError

-- | A way to log internal 'Event's
type Logger = Event -> IO ()

-- TODO. Add a Retrying Event
-- | @postgres@ is not ready until we are able to successfully connect.
--   'waitForDB' attempts to connect over and over again and returns
--   after the first successful connection.
waitForDB :: Logger -> Client.Options -> IO ()
waitForDB logger options = do
  logger TryToConnect
  let theConnectionString = Client.toConnectionString options
      startAction = PG.connectPostgreSQL theConnectionString
  try (bracket startAction PG.close mempty) >>= \case
    Left (_ :: IOError) -> threadDelay 10000 >> waitForDB logger options
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

prettyKeyPair ::(Pretty a, Pretty b) => a -> b -> Doc
prettyKeyPair k v = pretty k <> text ": " <> pretty v

instance Pretty ProcessConfig where
  pretty ProcessConfig {..}
    =  text "processConfigEnvVars:"
    <> softline
    <> indent 2 (vsep (map (uncurry prettyKeyPair) processConfigEnvVars))
    <> hardline
    <> text "processConfigCmdLine:"
    <> softline
    <> text (unwords processConfigCmdLine)

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

-- | Stop a 'ProcessHandle'. An alias for 'waitForProcess'
stopProcess :: ProcessHandle -> IO ExitCode
stopProcess = waitForProcess

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
-- | 'CompletePostgresPlan' is used be 'startPostgresProcess' to start the @postgres@
--   and then attempt to connect to it.
data CompletePostgresPlan = CompletePostgresPlan
  { completePostgresPlanProcessConfig :: ProcessConfig
  -- ^ The process config for @postgres@
  , completePostgresPlanClientOptions  :: Client.Options
  -- ^ Connection options. Used to verify that @postgres@ is ready.
  }

instance Pretty CompletePostgresPlan where
  pretty CompletePostgresPlan {..}
    =  text "completePostgresPlanProcessConfig:"
    <> softline
    <> indent 2 (pretty completePostgresPlanProcessConfig)
    <> hardline
    <> text "completePostgresPlanClientOptions:"
    <+> prettyOptions completePostgresPlanClientOptions

prettyOptions :: Client.Options -> Doc
prettyOptions = text . BSC.unpack . Client.toConnectionString

-- | The output of calling 'startPostgresProcess'.
data PostgresProcess = PostgresProcess
  { postgresProcessClientOptions :: Client.Options
  -- ^ Connection options
  , postgresProcessHandle :: ProcessHandle
  -- ^ @postgres@ process handle
  }

instance Pretty PostgresProcess where
  pretty PostgresProcess {..}
    =   text "postgresProcessClientOptions:"
    <+> prettyOptions postgresProcessClientOptions

-- Force all connections to the database to close.
-- Called during shutdown as well.
terminateConnections :: Client.Options-> IO ()
terminateConnections options = do
  let theConnectionString = Client.toConnectionString options
      terminationQuery = fromString $ unlines
        [ "SELECT pg_terminate_backend(pid)"
        , "FROM pg_stat_activity"
        , "WHERE datname=?;"
        ]
  e <- try $ bracket (PG.connectPostgreSQL theConnectionString) PG.close $
    \conn -> PG.execute conn terminationQuery
      [getLast $ Client.dbname options]
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
      terminateConnections postgresProcessClientOptions

      signalProcess sigINT p
    OpenExtHandle {} -> pure () -- TODO log windows is not supported
    ClosedHandle _ -> return ()

  waitForProcess postgresProcessHandle

-- | Start the @postgres@ process and block until a successful connection
--   occurs. A separate thread we continously check to see if the @postgres@
--   process has crashed.
startPostgresProcess :: Logger -> CompletePostgresPlan -> IO PostgresProcess
startPostgresProcess logger CompletePostgresPlan {..} = do
  logger StartPostgres

  let startAction = PostgresProcess completePostgresPlanClientOptions
        <$> startProcess "postgres" completePostgresPlanProcessConfig

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
      let options = completePostgresPlanClientOptions
            { Client.dbname = pure "template1"
            }

      -- Block until a connection succeeds or postgres crashes
      waitForDB logger options
        `race_` forever (checkForCrash >> threadDelay 100000)

      -- Postgres is now ready so return
      return result
-------------------------------------------------------------------------------
-- CompletePlan
-------------------------------------------------------------------------------
-- | 'CompletePlan' is the low level configuration necessary for creating a database
--   starting @postgres@ and creating a database. There is no validation done
--   on the 'CompletePlan'. It is recommend that one use the higher level functions
--   such as 'Database.Postgres.Temp.start' which will generate plans that
--   are valid. 'CompletePlan's are used internally but are exposed if the higher
--   level plan generation is not sufficent.
data CompletePlan = CompletePlan
  { completePlanLogger        :: Logger
  , completePlanInitDb        :: Maybe ProcessConfig
  , completePlanCreateDb      :: Maybe ProcessConfig
  , completePlanPostgres      :: CompletePostgresPlan
  , completePlanConfig        :: String
  , completePlanDataDirectory :: FilePath
  }

instance Pretty CompletePlan where
  pretty CompletePlan {..}
    =   text "completePlanInitDb:"
    <>  softline
    <>  indent 2 (pretty completePlanInitDb)
    <>  hardline
    <>  text "completePlanCreateDb:"
    <>  softline
    <>  indent 2 (pretty completePlanCreateDb)
    <>  hardline
    <>  text "completePlanPostgres:"
    <>  softline
    <>  indent 2 (pretty completePlanPostgres)
    <>  hardline
    <>  text "completePlanConfig:"
    <>  softline
    <>  indent 2 (pretty completePlanConfig)
    <>  hardline
    <>  text "completePlanDataDirectory:"
    <+> pretty completePlanDataDirectory

-- A simple helper to throw 'ExitCode's when they are 'ExitFailure'.
throwIfNotSuccess :: Exception e => (ExitCode -> e) -> ExitCode -> IO ()
throwIfNotSuccess f = \case
  ExitSuccess -> pure ()
  e -> throwIO $ f e

-- | 'startPlan' optionally calls @initdb@, optionally calls @createdb@ and
--   unconditionally calls @postgres@.
--   Additionally it writes a \"postgresql.conf\" and does not return until
--   the @postgres@ process is ready. See 'startPostgresProcess' for more
--   details.
startPlan :: CompletePlan -> IO PostgresProcess
startPlan plan@CompletePlan {..} = do
  completePlanLogger $ StartPlan $ show $ pretty plan
  for_ completePlanInitDb  $ executeProcess "initdb" >=>
    throwIfNotSuccess InitDbFailed

  -- We must provide a config file before we can start postgres.
  writeFile (completePlanDataDirectory <> "/postgresql.conf") completePlanConfig

  let startAction = startPostgresProcess completePlanLogger completePlanPostgres

  bracketOnError startAction stopPostgresProcess $ \result -> do
    for_ completePlanCreateDb $  executeProcess "createdb" >=>
      throwIfNotSuccess CreateDbFailed

    pure result

-- | Stop the @postgres@ process. See 'stopPostgresProcess' for more details.
stopPlan :: PostgresProcess -> IO ExitCode
stopPlan = stopPostgresProcess
