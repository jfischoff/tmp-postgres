{-# OPTIONS_HADDOCK prune #-}
{-|
This module provides the low level functionality for running @initdb@, @postgres@ and @createdb@ to make a database.

See 'startPlan' for more details.
-}
module Database.Postgres.Temp.Internal.Core where

import           Control.Concurrent
import           Control.Concurrent.Async (race_, withAsync)
import           Control.Exception
import           Control.Monad
import qualified Data.ByteString.Char8 as BSC
import           Data.Foldable (for_)
import           Data.IORef
import           Data.Maybe
import           Data.Typeable
import qualified Database.PostgreSQL.Simple as PG
import qualified Database.PostgreSQL.Simple.Options as Client
import           Prettyprinter
import           System.Directory
import           System.Exit (ExitCode(..))
import           System.IO
import           System.IO.Unsafe (unsafePerformIO)
import           System.Posix.Signals (sigINT, sigQUIT, signalProcess)
import           System.Process
import           System.Process.Internals
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
  = StartPostgresFailed ExitCode
  -- ^ @postgres@ failed before a connection succeeded. Most likely this
  --   is due to invalid configuration
  | InitDbFailed
    { startErrorStdOut   :: String
    , startErrorStdErr   :: String
    , startErrorExitCode :: ExitCode
    }
  -- ^ @initdb@ failed. This can be from invalid configuration or using a
  --   non-empty data directory
  | CreateDbFailed
    { startErrorStdOut   :: String
    , startErrorStdErr   :: String
    , startErrorExitCode :: ExitCode
    }
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
  | CopyCachedInitDbFailed String ExitCode
  -- ^ This is called if copying a folder cache fails.
  | FailedToFindDataDirectory String
  -- ^ Failed to find a data directory when trying to get
  --   a cached @initdb@ folder.
  | SnapshotCopyFailed String ExitCode
  -- ^ We tried to copy a data directory to a snapshot folder and it failed
  deriving (Show, Eq, Typeable)

instance Exception StartError

-- | A way to log internal 'Event's
--
--   @since 1.12.0.0
type Logger = Event -> IO ()

-- A simple helper to throw 'ExitCode's when they are 'ExitFailure'.
throwIfNotSuccess :: Exception e => (ExitCode -> e) -> ExitCode -> IO ()
throwIfNotSuccess f = \case
  ExitSuccess -> pure ()
  e -> throwIO $ f e

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

-- Only useful if we believe the output is finite
teeHandle :: Handle -> (Handle -> IO a) -> IO (a, String)
teeHandle orig f =
  bracket createPipe (\(x, y) -> hClose x >> hClose y) $ \(readEnd, writeEnd) -> do
    outputRef <- newIORef []

    let readerLoop = forever $ do
          theLine <- hGetLine readEnd
          modifyIORef outputRef (<>theLine)
          hPutStrLn orig theLine

    res <- withAsync readerLoop $ \_ -> f writeEnd
    (res,) <$> readIORef outputRef

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

-- | Start a process interactively and return the 'ProcessHandle'
startProcess
  :: String
  -- ^ Process name
  -> CompleteProcessConfig
  -- ^ Process config
  -> IO ProcessHandle
startProcess name CompleteProcessConfig {..} = (\(_, _, _, x) -> x) <$>
  createProcess_ name (proc name completeProcessConfigCmdLine)
    { std_err = UseHandle completeProcessConfigStdErr
    , std_out = UseHandle completeProcessConfigStdOut
    , std_in  = UseHandle completeProcessConfigStdIn
    , env     = Just completeProcessConfigEnvVars
    , create_group = completeProcessConfigCreateGroup
    }

-- | Stop a 'ProcessHandle'. An alias for 'waitForProcess'
stopProcess :: ProcessHandle -> IO ExitCode
stopProcess = waitForProcess

-- | Start a process and block until it finishes return the 'ExitCode'.
executeProcess
  :: String
  -- ^ Process name
  -> CompleteProcessConfig
  -- ^ Process config
  -> IO ExitCode
executeProcess name conf =
  bracket (startProcess name conf) terminateProcess waitForProcess

-- | Start a process and block until it finishes return the 'ExitCode' and the
--   stderr output.
executeProcessAndTee
  :: String
  -- ^ Process name
  -> CompleteProcessConfig
  -- ^ Process config
  -> IO (ExitCode, String, String)
executeProcessAndTee name config = fmap (\((x, y), z) -> (x, z, y)) $
  teeHandle (completeProcessConfigStdOut config) $ \newOut ->
    teeHandle (completeProcessConfigStdErr config) $ \newErr ->
      executeProcess name $ config
        { completeProcessConfigStdErr = newErr
        , completeProcessConfigStdOut = newOut
        }
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
  , postgresProcessHandle :: ProcessHandle
  -- ^ @postgres@ process handle
  }

instance Pretty PostgresProcess where
  pretty PostgresProcess {..}
    =   "postgresProcessClientOptions:"
    <+> prettyOptions postgresProcessClientOptions

-- | Stop the @postgres@ process after attempting to terminate all the
--   connections.
stopPostgresProcess :: Bool -> PostgresProcess -> IO ExitCode
stopPostgresProcess graceful PostgresProcess{..} = do
  withProcessHandle postgresProcessHandle $ \case
    OpenHandle p   ->
      -- Call for "Immediate shutdown"
      signalProcess (if graceful then sigINT else sigQUIT) p
    OpenExtHandle {} -> pure () -- TODO log windows is not supported
    ClosedHandle _ -> return ()

  waitForProcess postgresProcessHandle

-- | Start the @postgres@ process and block until a successful connection
--   occurs. A separate thread we continously check to see if the @postgres@
--   process has crashed.
startPostgresProcess :: Int -> Logger -> CompletePostgresPlan -> IO PostgresProcess
startPostgresProcess time logger CompletePostgresPlan {..} = do
  logger StartPostgres

  let startAction = PostgresProcess completePostgresPlanClientOptions
        <$> startProcess "postgres" completePostgresPlanProcessConfig

  -- Start postgres and stop if an exception occurs
  bracketOnError startAction (stopPostgresProcess False) $
    \result@PostgresProcess {..} -> do
      logger WaitForDB
      -- We assume that 'template1' exist and make connection
      -- options to test if postgres is ready.
      let options = completePostgresPlanClientOptions
            { Client.dbname = pure "template1"
            }

           -- A helper to check if the process has died
          checkForCrash = do
            mExitCode <- getProcessExitCode postgresProcessHandle
            for_ mExitCode (throwIO . StartPostgresFailed)

          timeoutAndThrow = timeout time (waitForDB logger options) >>= \case
            Just () -> pure ()
            Nothing -> throwIO ConnectionTimedOut

      -- Block until a connection succeeds, postgres crashes or we timeout
      timeoutAndThrow `race_` forever (checkForCrash >> threadDelay 100000)

      -- Postgres is now ready so return
      return result

-------------------------------------------------------------------------------
-- Non interactive subcommands
-------------------------------------------------------------------------------
executeInitDb :: CompleteProcessConfig -> IO ()
executeInitDb config = do
  (res, stdOut, stdErr) <- executeProcessAndTee "initdb" config
  throwIfNotSuccess (InitDbFailed stdOut stdErr) res

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
#ifdef darwin_HOST_OS
    cpFlags = if copyDirectoryCommandCow then "cp -Rc " else "cp -R "
#else
    cpFlags = if copyDirectoryCommandCow then "cp -R --reflink=auto " else "cp -R "
#endif
    copyCommand = cpFlags <> copyDirectoryCommandSrc <> "/* " <> copyDirectoryCommandDst
  throwIfNotSuccess (CopyCachedInitDbFailed copyCommand) =<< system copyCommand

-- | Call @createdb@ and tee the output to return if there is an
--   an exception. Throws 'CreateDbFailed'.
executeCreateDb :: CompleteProcessConfig -> IO ()
executeCreateDb config = do
  (res, stdOut, stdErr) <- executeProcessAndTee "createdb" config
  throwIfNotSuccess (CreateDbFailed stdOut stdErr) res

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
