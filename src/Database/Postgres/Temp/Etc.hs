module Database.Postgres.Temp.Etc where
import GHC.Generics (Generic)
import Data.Typeable (Typeable)
import Control.Exception
import Control.Concurrent
import Data.Monoid
import qualified Database.PostgreSQL.Simple.Options as PostgresClient
import qualified Database.PostgreSQL.Simple as PG
import System.IO
import Data.Monoid.Generic
import System.Process
import System.Exit
import Control.Monad ((<=<))
import System.Environment
import System.Directory
import System.IO.Temp (createTempDirectory)
import Control.Applicative
import Data.Maybe

fourth :: (a, b, c, d) -> d
fourth (_, _, _, x) = x

throwMaybe :: Exception e => e -> Maybe a -> IO a
throwMaybe e = \case
  Nothing -> throwIO e
  Just  x -> pure x

-- A helper for dealing with locks
withLock :: MVar a -> IO b -> IO b
withLock m f = withMVar m (const f)

rmDirIgnoreErrors :: FilePath -> IO ()
rmDirIgnoreErrors mainDir =
  removeDirectoryRecursive mainDir `catch` (\(_ :: IOException) -> return ())

waitForDB :: PostgresClient.Options -> IO ()
waitForDB options = do
  let theConnectionString = PostgresClient.toConnectionString options
  try (bracket (PG.connectPostgreSQL theConnectionString) PG.close mempty) >>= \case
    Left (_ :: IOError) -> threadDelay 10000 >> waitForDB options
    Right () -> return ()
-------------------------------------------------------------------------------
-- A useful type of options
-------------------------------------------------------------------------------
data Lastoid a = Replace a | Mappend a

instance Semigroup a => Semigroup (Lastoid a) where
  x <> y = case (x, y) of
    (r@Replace {}, _        ) -> r
    (Mappend a   , Replace b) -> Replace $ a <> b
    (Mappend a   , Mappend b) -> Mappend $ a <> b

instance Monoid a => Monoid (Lastoid a) where
  mempty = Mappend mempty
  mappend = (<>)
-------------------------------------------------------------------------------
-- PartialProcessOptions
-------------------------------------------------------------------------------
data PartialProcessOptions = PartialProcessOptions
  { partialProcessOptionsEnvVars :: Lastoid [(String, String)]
  , partialProcessOptionsCmdLine :: Lastoid [String]
  , partialProcessOptionsStdIn   :: Last Handle
  , partialProcessOptionsStdOut  :: Last Handle
  , partialProcessOptionsStdErr  :: Last Handle
  , partialProcessOptionsName    :: Last String
  }
  deriving stock (Generic)
  deriving Semigroup via GenericSemigroup PartialProcessOptions
  deriving Monoid    via GenericMonoid PartialProcessOptions

standardProcessOptions :: IO PartialProcessOptions
standardProcessOptions = do
  env <- getEnvironment
  pure mempty
    { partialProcessOptionsEnvVars = Replace env
    , partialProcessOptionsStdIn   = pure stdin
    , partialProcessOptionsStdOut  = pure stdout
    , partialProcessOptionsStdErr  = pure stderr
    }
-------------------------------------------------------------------------------
-- ProcessOptions
-------------------------------------------------------------------------------
data ProcessOptions = ProcessOptions
  { processOptionsEnvVars :: [(String, String)]
  , processOptionsCmdLine :: [String]
  , processOptionsStdIn   :: Handle
  , processOptionsStdOut  :: Handle
  , processOptionsStdErr  :: Handle
  , processOptionsName    :: String
  }

completeProcessOptions :: PartialProcessOptions -> Maybe ProcessOptions
completeProcessOptions PartialProcessOptions {..} = do
  processOptionsName <- getLast partialProcessOptionsName
  processOptionsEnvVars <- case partialProcessOptionsEnvVars of
    Replace x -> Just x
    Mappend _ -> Nothing
  processOptionsCmdLine <- case partialProcessOptionsCmdLine of
    Replace x -> pure x
    Mappend _ -> Nothing
  processOptionsStdIn  <- getLast partialProcessOptionsStdIn
  processOptionsStdOut <- getLast partialProcessOptionsStdOut
  processOptionsStdErr <- getLast partialProcessOptionsStdErr

  pure ProcessOptions {..}
-------------------------------------------------------------------------------
-- ProcessInput
-------------------------------------------------------------------------------
data ProcessInput = ProcessInput
  { processInputEnvVars :: [(String, String)]
  , processInputCmdLine :: [String]
  , processInputName    :: String
  } deriving (Show, Eq, Ord)

-- envs might not be true
toProcessInput :: ProcessOptions -> ProcessInput
toProcessInput ProcessOptions {..} = ProcessInput
  { processInputEnvVars = processOptionsEnvVars
  , processInputCmdLine = processOptionsCmdLine
  , processInputName    = processOptionsName
  }

evaluateProcess :: ProcessOptions -> IO ProcessHandle
evaluateProcess ProcessOptions {..} = fmap fourth $
  createProcess_ processOptionsName $
    (proc processOptionsName processOptionsCmdLine)
      { std_err = UseHandle processOptionsStdErr
      , std_out = UseHandle processOptionsStdOut
      , std_in  = UseHandle processOptionsStdIn
      , env     = Just processOptionsEnvVars
      }

executeProcess :: ProcessOptions -> IO ExitCode
executeProcess = waitForProcess <=< evaluateProcess
-------------------------------------------------------------------------------
-- DirectoryType
-------------------------------------------------------------------------------
data DirectoryType = Permanent FilePath | Temporary FilePath
  deriving(Show, Eq, Ord)

toFilePath :: DirectoryType -> FilePath
toFilePath = \case
  Permanent x -> x
  Temporary x -> x

initializeDirectoryType :: String -> Maybe FilePath -> IO DirectoryType
initializeDirectoryType pattern = \case
  Nothing -> Temporary <$> createTempDirectory "/tmp" pattern
  Just x  -> pure $ Permanent x

cleanupDirectoryType :: DirectoryType -> IO ()
cleanupDirectoryType = \case
  Permanent _ -> pure ()
  Temporary filePath -> rmDirIgnoreErrors filePath
-------------------------------------------------------------------------------
-- PartialSocketClass
-------------------------------------------------------------------------------
data PartialSocketClass =
  PIpSocket (Maybe String) | PUnixSocket (Maybe FilePath)
    deriving stock (Show, Eq, Read, Ord, Generic, Typeable)

instance Semigroup PartialSocketClass where
  x <> y = case (x, y) of
    (PIpSocket   a, PIpSocket b) -> PIpSocket $ a <|> b
    (a@(PIpSocket _), PUnixSocket _) -> a
    (PUnixSocket _, a@(PIpSocket _)) -> a
    (PUnixSocket a, PUnixSocket b) -> PUnixSocket $ a <|> b

instance Monoid PartialSocketClass where
 mempty = PUnixSocket Nothing

isTempSocket :: PartialSocketClass -> Bool
isTempSocket = \case
  PUnixSocket mFilePath -> isNothing mFilePath
  _ -> False
-------------------------------------------------------------------------------
-- SocketClass
-------------------------------------------------------------------------------
data SocketClass = IpSocket String | UnixSocket DirectoryType
  deriving (Show, Eq, Ord, Generic, Typeable)

listenAddressConfig :: SocketClass -> [String]
listenAddressConfig = \case
  IpSocket ip    -> ["listen_addresses = '" <> ip <> "'"]
  UnixSocket dir ->
    [ "listen_addresses = ''"
    , "unix_socket_directories = '" <> toFilePath dir <> "'"
    ]

socketClassToHost :: SocketClass -> String
socketClassToHost = \case
  IpSocket ip    -> ip
  UnixSocket dir -> toFilePath dir

startPartialSocketClass
  :: PartialSocketClass
  -> (SocketClass -> IO a)
  -> IO a
startPartialSocketClass theClass f = case theClass of
  PIpSocket mIp -> f $ IpSocket $ fromMaybe "127.0.0.1" mIp
  PUnixSocket mFilePath ->
    bracketOnError (initializeDirectoryType "tmp-postgres-socket" mFilePath)
      cleanupDirectoryType $ \socketPath -> f $ UnixSocket socketPath

stopSocketOptions :: SocketClass -> IO ()
stopSocketOptions = \case
  IpSocket   {}  -> pure ()
  UnixSocket dir -> cleanupDirectoryType dir