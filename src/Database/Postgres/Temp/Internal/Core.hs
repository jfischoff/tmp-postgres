{-# OPTIONS_HADDOCK prune #-}
{-|
This module provides the low level functionality for running @initdb@, @postgres@ and @createdb@ to make a database.

See 'startPlan' for more details.
-}
module Database.Postgres.Temp.Internal.Core where

import           Control.Concurrent
import           Control.Concurrent.Async (race_)
import           Control.Exception
import           Control.Monad
import qualified Data.ByteString.Char8 as BSC
import           Data.ByteString.Lazy (ByteString)
import           Data.Foldable (for_)
import           Data.Maybe
import           Data.Typeable
import qualified Database.PostgreSQL.Simple as PG
import qualified Database.PostgreSQL.Simple.Options as Client
import           GHC.Conc (STM, atomically, throwSTM)
import           GHC.Stack (HasCallStack)
import           Prettyprinter
import           System.Directory
import           System.IO (Handle)
import           System.IO.Unsafe (unsafePerformIO)
import           System.Posix.Signals (sigINT, sigQUIT, signalProcess)
import qualified System.Process (getPid)
import           System.Process.Typed
import           System.Timeout

-- | Internal events for debugging
--
--   @since 1.12.0.0
data Event
  = StartPlan String
  -- ^ The first event. This useful for debugging
  --   what is actual passed to the @initdb@, @createdb@ and
  --   @postgres@.
  | StartPostgres
  -- ^ The second event. Postgres is about to get called
  | WaitForDB
  -- ^ The third event. Postgres started. We are now about to
  -- setup a reconnect loop (racing with a process checker)
  | TryToConnect
  -- ^ The fourth event and (possibly all subsequent events).
  -- We are looping trying to successfully connect to the @postgres@
  -- process.
  deriving (Eq, Ord)

instance Show Event where
  show = \case
    StartPlan x -> "StartPlan:\n" <> x
    StartPostgres -> "StartPostgres"
    WaitForDB -> "WaitForDB"
    TryToConnect -> "TryToConnect"

-- | A list of failures that can occur when starting. This is not
--   and exhaustive list but covers the errors that the system
--   catches for the user.
--
--   @since 1.29.0.0
data StartError
  = StartPostgresFailed ExitCodeException
  -- ^ @postgres@ failed before a connection succeeded. Most likely this
  --   is due to invalid configuration
  | InitDbFailed ExitCodeException
  -- ^ @initdb@ failed. This can be from invalid configuration or using a
  --   non-empty data directory
  | CreateDbFailed ExitCodeException
  -- ^ @createdb@ failed. This can be from invalid configuration or
  --   the database might already exist.
  | PlanFailed String [String]
  -- ^ The 'Database.Postgres.Temp.Config.Plan' was missing info and a complete 'Plan' could
  --   not be created.
  | CompleteProcessConfigFailed String [String]
  -- ^ The 'Database.Postgres.Temp.Config.ProcessConfig' was missing info and a
  -- 'CompleteProcessConfig' could not be created.
  | ConnectionTimedOut
  -- ^ Timed out waiting for @postgres@ to accept a connection
  | DeleteDbError PG.SqlError
  | EmptyDataDirectory
  -- ^ This will happen if a 'Database.Postgres.Temp.Config.Plan' is missing a
  --   'Database.Postgres.Temp.Config.initDbConfig'.
  | CopyCachedInitDbFailed ExitCodeException
  -- ^ This is called if copying a folder cache fails.
  | FailedToFindDataDirectory String
  -- ^ Failed to find a data directory when trying to get
  --   a cached @initdb@ folder.
  | SnapshotCopyFailed ExitCodeException
  -- ^ We tried to copy a data directory to a snapshot folder and it failed
  deriving (Show, Typeable)

instance Exception StartError

-- | A way to log internal 'Event's
--
--   @since 1.12.0.0
type Logger = Event -> IO ()

-- | @postgres@ is not ready until we are able to successfully connect.
--   'waitForDB' attempts to connect over and over again and returns
--   after the first successful connection.
waitForDB :: Logger -> Client.Options -> IO ()
waitForDB logger options = do
  logger TryToConnect
  let theConnectionString = Client.toConnectionString options
      startAction = PG.connectPostgreSQL theConnectionString
  try (bracket startAction PG.close mempty) >>= \case
    Left (_ :: IOError) -> threadDelay 1000 >> waitForDB logger options
    Right () -> return ()

-- | 'CompleteProcessConfig' contains the configuration necessary for starting a
--   process. It is essentially a stripped down 'System.Process.CreateProcess'.
data CompleteProcessConfig = CompleteProcessConfig
  { completeProcessConfigEnvVars     :: [(String, String)]
  -- ^ Environment variables
  , completeProcessConfigCmdLine     :: [String]
  -- ^ Command line arguements
  , completeProcessConfigStdIn       :: Handle
  -- ^ The 'Handle' for standard input
  , completeProcessConfigStdOut      :: Handle
  -- ^ The 'Handle' for standard output
  , completeProcessConfigStdErr      :: Handle
  -- ^ The 'Handle' for standard error
  , completeProcessConfigCreateGroup :: Bool
  -- ^ Whether or not to create new process group
  }

prettyHandle :: Handle -> Doc ann
prettyHandle _ = "HANDLE"

prettyKeyPair ::(Pretty a, Pretty b) => a -> b -> Doc ann
prettyKeyPair k v = pretty k <> ": " <> pretty v

instance Pretty CompleteProcessConfig where
  pretty CompleteProcessConfig {..}
    =  "completeProcessConfigEnvVars:"
    <> softline
    <> indent 2 (vsep (map (uncurry prettyKeyPair) completeProcessConfigEnvVars))
    <> hardline
    <> "completeProcessConfigCmdLine:"
    <> softline
    <> pretty (unwords completeProcessConfigCmdLine)
    <> hardline
    <> "completeProcessConfigStdIn:"
    <+> prettyHandle completeProcessConfigStdIn
    <> hardline
    <> "completeProcessConfigStdOut:"
    <+> prettyHandle completeProcessConfigStdOut
    <> hardline
    <> "completeProcessConfigStdErr:"
    <+> prettyHandle completeProcessConfigStdErr
    <> hardline
    <> "completeProcessConfigCreateGroup:"
    <> softline
    <> pretty completeProcessConfigCreateGroup

-------------------------------------------------------------------------------
-- PostgresProcess Life cycle management
-------------------------------------------------------------------------------
-- | 'CompletePostgresPlan' is used be 'startPostgresProcess' to start the
--   @postgres@ and then attempt to connect to it.
data CompletePostgresPlan = CompletePostgresPlan
  { completePostgresPlanProcessConfig :: CompleteProcessConfig
  -- ^ The process config for @postgres@
  , completePostgresPlanClientOptions  :: Client.Options
  -- ^ Connection options. Used to verify that @postgres@ is ready.
  }

instance Pretty CompletePostgresPlan where
  pretty CompletePostgresPlan {..}
    =  "completePostgresPlanProcessConfig:"
    <> softline
    <> indent 2 (pretty completePostgresPlanProcessConfig)
    <> hardline
    <> "completePostgresPlanClientOptions:"
    <+> prettyOptions completePostgresPlanClientOptions

prettyOptions :: Client.Options -> Doc ann
prettyOptions = pretty . BSC.unpack . Client.toConnectionString

-- | The output of calling 'startPostgresProcess'.
data PostgresProcess = PostgresProcess
  { postgresProcessClientOptions :: Client.Options
  -- ^ Connection options
  , postgresProcessHandle :: Process () (STM ByteString) (STM ByteString)
  -- ^ @postgres@ process handle
  , postgresProcessConfig :: ProcessConfig () () ()
  -- ^ @postgres@ process config
  }

instance Pretty PostgresProcess where
  pretty PostgresProcess {..}
    =   "postgresProcessClientOptions:"
    <+> prettyOptions postgresProcessClientOptions

-- | Stop the @postgres@ process after attempting to terminate all the
--   connections.
stopPostgresProcess :: Bool -> PostgresProcess -> IO ExitCode
stopPostgresProcess graceful PostgresProcess{..} = do
  maybeExitCode <- getExitCode postgresProcessHandle
  case maybeExitCode of
    -- Already exited.
    Just _ -> return ()
    Nothing -> do
      -- Call for "Immediate shutdown"
      -- NB: typed-process 0.2.12.0 introduced a `getPid` function.
      maybePid <- System.Process.getPid $ unsafeProcessHandle postgresProcessHandle
      forM_ maybePid $ signalProcess $ if graceful then sigINT else sigQUIT

  waitExitCode postgresProcessHandle

-- | Start the @postgres@ process and block until a successful connection
--   occurs. A separate thread we continously check to see if the @postgres@
--   process has crashed.
startPostgresProcess :: HasCallStack => Int -> Logger -> CompletePostgresPlan -> IO PostgresProcess
startPostgresProcess time logger CompletePostgresPlan {..} = do
  logger StartPostgres

  let processConfig =
        setStdout byteStringOutput
        $ setStderr byteStringOutput
        $ setEnv (completeProcessConfigEnvVars completePostgresPlanProcessConfig)
        $ setCreateGroup (completeProcessConfigCreateGroup completePostgresPlanProcessConfig)
        $ proc "postgres" (completeProcessConfigCmdLine completePostgresPlanProcessConfig)

      startAction = do
        processHandle <- startProcess processConfig
        pure
          $ PostgresProcess
            { postgresProcessClientOptions = completePostgresPlanClientOptions
            , postgresProcessHandle = processHandle
            , postgresProcessConfig = clearStreams processConfig
            }

      -- We assume that 'template1' exist and make connection
      -- options to test if postgres is ready.
      options = completePostgresPlanClientOptions
        { Client.dbname = pure "template1"
        }

  -- Start postgres and stop if an exception occurs
  bracketOnError startAction (stopPostgresProcess False) $
    \result@PostgresProcess{..} -> do
      logger WaitForDB
      -- A helper to check if the process has died
      let checkForCrash = do
            maybeExitCode <- getExitCode postgresProcessHandle
            case maybeExitCode of
              Nothing -> pure ()
              Just exitCode -> atomically $ do
                stdout <- getStdout postgresProcessHandle
                stderr <- getStderr postgresProcessHandle
                throwSTM $ StartPostgresFailed $ ExitCodeException
                  { eceExitCode = exitCode
                  , eceProcessConfig = postgresProcessConfig
                  , eceStdout = stdout
                  , eceStderr = stderr
                  }

          timeoutAndThrow = timeout time (waitForDB logger options) >>= \case
            Just () -> pure ()
            Nothing -> throwIO ConnectionTimedOut

      -- Block until a connection succeeds, postgres crashes or we timeout
      timeoutAndThrow `race_` forever (checkForCrash >> threadDelay 100000)

      -- Postgres is now ready so return
      return result

clearStreams :: ProcessConfig stdin stdout stderr -> ProcessConfig () () ()
clearStreams =
  setStdout inherit
  . setStderr inherit
  . setStdin inherit

-------------------------------------------------------------------------------
-- Non interactive subcommands
-------------------------------------------------------------------------------
executeInitDb :: CompleteProcessConfig -> IO ()
executeInitDb config = do
  (void
    $ readProcess_
    $ setEnv (completeProcessConfigEnvVars config)
    $ setCreateGroup (completeProcessConfigCreateGroup config)
    $ proc "initdb" (completeProcessConfigCmdLine config))
    `catch` (throwIO . InitDbFailed)

data CompleteCopyDirectoryCommand = CompleteCopyDirectoryCommand
  { copyDirectoryCommandSrc :: FilePath
  , copyDirectoryCommandDst :: FilePath
  , copyDirectoryCommandCow :: Bool
  } deriving (Show, Eq, Ord)

instance Pretty CompleteCopyDirectoryCommand where
  pretty CompleteCopyDirectoryCommand {..}
    =  "copyDirectoryCommandSrc:"
    <> softline
    <> indent 2 (pretty copyDirectoryCommandSrc)
    <> hardline
    <> "copyDirectoryCommandDst:"
    <> softline
    <> indent 2 (pretty copyDirectoryCommandDst)
    <> hardline
    <> "copyDirectoryCommandCow:"
    <+> pretty copyDirectoryCommandCow

executeCopyDirectoryCommand :: CompleteCopyDirectoryCommand -> IO ()
executeCopyDirectoryCommand CompleteCopyDirectoryCommand {..} = do
  let
    cpFlags =
      ["-R"]
#ifdef darwin_HOST_OS
      ++ if copyDirectoryCommandCow then ["-c"] else []
#else
      ++ if copyDirectoryCommandCow then ["--reflink=auto"] else []
#endif
      ++ [copyDirectoryCommandSrc, copyDirectoryCommandDst]
    copyCommand = proc "cp" cpFlags

  (void $ readProcess_ copyCommand)
    `catch` (throwIO . CopyCachedInitDbFailed)

-- | Call @createdb@ and tee the output to return if there is an
--   an exception. Throws 'CreateDbFailed'.
executeCreateDb :: CompleteProcessConfig -> IO ()
executeCreateDb config = do
  (void
    $ readProcess_
    $ setEnv (completeProcessConfigEnvVars config)
    $ setCreateGroup (completeProcessConfigCreateGroup config)
    $ proc "createdb" (completeProcessConfigCmdLine config))
    `catch` (throwIO . CreateDbFailed)

-- The DataDirectory and the initdb data directory must match!
data InitDbCachePlan = InitDbCachePlan
  { cachePlanDataDirectory :: FilePath
  , cachePlanInitDb        :: CompleteProcessConfig
  , cachePlanCopy          :: CompleteCopyDirectoryCommand
  }

instance Pretty InitDbCachePlan where
  pretty InitDbCachePlan {..}
    =   "cachePlanDataDirectory:"
    <>  softline
    <>  indent 2 (pretty cachePlanDataDirectory)
    <>  hardline
    <>  "cachePlanInitDb:"
    <>  softline
    <>  indent 2 (pretty cachePlanInitDb)
    <>  hardline
    <>  "cachePlanCopy:"
    <>  softline
    <>  indent 2 (pretty cachePlanCopy)

cacheLock :: MVar ()
cacheLock = unsafePerformIO $ newMVar ()
{-# NOINLINE cacheLock #-}

executeInitDbCachePlan :: InitDbCachePlan -> IO ()
executeInitDbCachePlan InitDbCachePlan {..} = do
  withMVar cacheLock $ \_ -> do
    -- Check if the data directory exists
    exists <- doesDirectoryExist cachePlanDataDirectory
    -- If it does not call initdb
    unless exists $ executeInitDb cachePlanInitDb
    -- call the copy

  executeCopyDirectoryCommand cachePlanCopy
-------------------------------------------------------------------------------
-- Plan
-------------------------------------------------------------------------------
-- | 'Plan' is the low level configuration necessary for initializing
--   a database cluster
--   starting @postgres@ and creating a database. There is no validation done
--   on the 'Plan'. It is recommend that one use the higher level
--   functions
--   such as 'Database.Postgres.Temp.start' which will generate plans that
--   are valid. 'Plan's are used internally but are exposed if the
--   higher level plan generation is not sufficent.
data Plan = Plan
  { completePlanLogger            :: Logger
  , completePlanInitDb            :: Maybe (Either CompleteProcessConfig InitDbCachePlan)
  , completePlanCopy              :: Maybe CompleteCopyDirectoryCommand
  , completePlanCreateDb          :: Maybe CompleteProcessConfig
  , completePlanPostgres          :: CompletePostgresPlan
  , completePlanConfig            :: String
  , completePlanDataDirectory     :: FilePath
  , completePlanConnectionTimeout :: Int
  }

eitherPretty :: (Pretty a, Pretty b) => Either a b -> Doc ann
eitherPretty = either pretty pretty

instance Pretty Plan where
  pretty Plan {..}
    =   "completePlanInitDb:"
    <>  softline
    <>  indent 2 (fromMaybe mempty $ fmap eitherPretty completePlanInitDb)
    <>  hardline
    <>  "completePlanCopy:"
    <>  softline
    <>  indent 2 (pretty completePlanCopy)
    <>  hardline
    <>  "completePlanCreateDb:"
    <>  softline
    <>  indent 2 (pretty completePlanCreateDb)
    <>  hardline
    <>  "completePlanPostgres:"
    <>  softline
    <>  indent 2 (pretty completePlanPostgres)
    <>  hardline
    <>  "completePlanConfig:"
    <>  softline
    <>  indent 2 (pretty completePlanConfig)
    <>  hardline
    <>  "completePlanDataDirectory:"
    <+> pretty completePlanDataDirectory

-- | 'startPlan' optionally calls @initdb@, optionally calls @createdb@ and
--   unconditionally calls @postgres@.
--   Additionally it writes a \"postgresql.conf\" and does not return until
--   the @postgres@ process is ready. See 'startPostgresProcess' for more
--   details.
startPlan :: Plan -> IO PostgresProcess
startPlan plan@Plan {..} = do
  completePlanLogger $ StartPlan $ show $ pretty plan
  for_ completePlanInitDb $ either executeInitDb executeInitDbCachePlan

  for_ completePlanCopy executeCopyDirectoryCommand

  -- Try to give a better error if @initdb@ was not
  -- configured to run.
  versionFileExists <- doesFileExist $ completePlanDataDirectory <> "/PG_VERSION"
  unless versionFileExists $ throwIO EmptyDataDirectory

  -- We must provide a config file before we can start postgres.
  writeFile (completePlanDataDirectory <> "/postgresql.conf") completePlanConfig

  let startAction = startPostgresProcess
        completePlanConnectionTimeout completePlanLogger completePlanPostgres

  bracketOnError startAction (stopPostgresProcess False) $ \result -> do
    for_ completePlanCreateDb executeCreateDb

    pure result

-- | Stop the @postgres@ process. See 'stopPostgresProcess' for more details.
stopPlan :: PostgresProcess -> IO ExitCode
stopPlan = stopPostgresProcess False
