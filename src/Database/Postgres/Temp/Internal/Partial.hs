module Database.Postgres.Temp.Internal.Partial where
import Database.Postgres.Temp.Internal.Core
import qualified Database.PostgreSQL.Simple.PartialOptions as Client
import GHC.Generics (Generic)
import Data.Monoid.Generic
import Data.Monoid
import Data.Typeable
import System.IO
import System.Environment
import Data.Maybe
import Control.Exception
import System.IO.Temp (createTempDirectory)
import Network.Socket.Free (getFreePort)
import Control.Monad (join)
import System.Directory
import Data.Either.Validation
import Control.Monad.Trans.Cont
import Control.Monad.Trans.Class
import Control.Applicative

-- TODO make functions for creating plans from PartialProcessConfig from PartialClientConfig
-- in the next version
-------------------------------------------------------------------------------
-- A useful type of options
-------------------------------------------------------------------------------
-- | 'Lastoid' is helper for overriding configuration values.
--   It's 'Semigroup' instance let's one either combine the
--   'a' of two 'Lastoid's using '(<>)' via the 'Mappend' constructor
--   or one can wholly replace the value with the last value using the 'Replace'
--   constructor.
data Lastoid a = Replace a | Mappend a
  deriving (Show, Eq, Functor)

instance Semigroup a => Semigroup (Lastoid a) where
  x <> y = case (x, y) of
    (_        , r@Replace {}) -> r
    (Replace a, Mappend   b ) -> Replace $ a <> b
    (Mappend a, Mappend   b ) -> Mappend $ a <> b

instance Monoid a => Monoid (Lastoid a) where
  mempty = Mappend mempty

getLastoid :: Lastoid a -> a
getLastoid = \case
  Replace a -> a
  Mappend a -> a
-------------------------------------------------------------------------------
-- PartialProcessConfig
-------------------------------------------------------------------------------
-- | The monoidial version of 'ProcessConfig'. Used to combine overrides with
--   defaults when creating a 'ProcessConfig'.
data PartialProcessConfig = PartialProcessConfig
  { partialProcessConfigEnvVars :: Lastoid [(String, String)]
  , partialProcessConfigCmdLine :: Lastoid [String]
  , partialProcessConfigStdIn   :: Last Handle
  , partialProcessConfigStdOut  :: Last Handle
  , partialProcessConfigStdErr  :: Last Handle
  }
  deriving stock (Generic)
  deriving Semigroup via GenericSemigroup PartialProcessConfig
  deriving Monoid    via GenericMonoid PartialProcessConfig

-- | The 'standardProcessConfig' sets the handles to 'stdin', 'stdout' and
--   'stderr' and inherits the environment variables from the calling
--   process.
standardProcessConfig :: IO PartialProcessConfig
standardProcessConfig = do
  env <- getEnvironment
  pure mempty
    { partialProcessConfigEnvVars = Replace env
    , partialProcessConfigStdIn   = pure stdin
    , partialProcessConfigStdOut  = pure stdout
    , partialProcessConfigStdErr  = pure stderr
    }

addErrorContext :: String -> Either [String] a -> Either [String] a
addErrorContext cxt = either (Left . map (cxt <>)) Right

getOption :: String -> Last a -> Validation [String] a
getOption optionName = \case
    Last (Just x) -> pure x
    Last Nothing  -> Failure ["Missing " ++ optionName ++ " option"]

completeProcessConfig :: PartialProcessConfig -> Either [String] ProcessConfig
completeProcessConfig PartialProcessConfig {..} = validationToEither $ do
  let processConfigEnvVars = getLastoid partialProcessConfigEnvVars
      processConfigCmdLine = getLastoid partialProcessConfigCmdLine
  processConfigStdIn  <- getOption "partialProcessConfigStdIn" partialProcessConfigStdIn
  processConfigStdOut <- getOption "partialProcessConfigStdOut" partialProcessConfigStdOut
  processConfigStdErr <- getOption "partialProcessConfigStdErr" partialProcessConfigStdErr

  pure ProcessConfig {..}
-------------------------------------------------------------------------------
-- DirectoryType
-------------------------------------------------------------------------------
-- | A type to track whether a file is temporary and needs to be cleaned up.
data DirectoryType = Permanent FilePath | Temporary FilePath
  deriving(Show, Eq, Ord)

toFilePath :: DirectoryType -> FilePath
toFilePath = \case
  Permanent x -> x
  Temporary x -> x
-------------------------------------------------------------------------------
-- PartialDirectoryType
-------------------------------------------------------------------------------
-- | The monoidial version of 'DirectoryType'. Used to combine overrides with
--   defaults when creating a 'DirectoryType'.
data PartialDirectoryType = PPermanent FilePath | PTemporary
  deriving(Show, Eq, Ord)

instance Semigroup PartialDirectoryType where
  x <> y = case (x, y) of
    (PTemporary     , a   ) -> a
    (a@PPermanent {}, _   ) -> a

instance Monoid PartialDirectoryType where
  mempty = PTemporary

initDirectoryType :: String -> PartialDirectoryType -> IO DirectoryType
initDirectoryType pattern = \case
  PTemporary -> Temporary <$> createTempDirectory "/tmp" pattern
  PPermanent x  -> pure $ Permanent x

rmDirIgnoreErrors :: FilePath -> IO ()
rmDirIgnoreErrors mainDir =
  removeDirectoryRecursive mainDir `catch` (\(_ :: IOException) -> return ())

shutdownDirectoryType :: DirectoryType -> IO ()
shutdownDirectoryType = \case
  Permanent _ -> pure ()
  Temporary filePath -> rmDirIgnoreErrors filePath
-------------------------------------------------------------------------------
-- SocketClass
-------------------------------------------------------------------------------
-- | A type for configuring the listening address of the @postgres@ process.
--   @postgres@ can listen on several types of sockets simulatanously but we
--   don't support that behavior. One can either listen on a IP based socket
--   or a UNIX domain socket.
data SocketClass = IpSocket String | UnixSocket DirectoryType
  deriving (Show, Eq, Ord, Generic, Typeable)

-- | Create the extra config lines for listening based on the 'SocketClass'
socketClassToConfig :: SocketClass -> [String]
socketClassToConfig = \case
  IpSocket ip    -> ["listen_addresses = '" <> ip <> "'"]
  UnixSocket dir ->
    [ "listen_addresses = ''"
    , "unix_socket_directories = '" <> toFilePath dir <> "'"
    ]

-- | Many processes require a \"host\" flag. We can generate one from the
--   'SocketClass'.
socketClassToHostFlag :: SocketClass -> [String]
socketClassToHostFlag x = ["-h", socketClassToHost x]

socketClassToHost :: SocketClass -> String
socketClassToHost = \case
  IpSocket ip    -> ip
  UnixSocket dir -> toFilePath dir
-------------------------------------------------------------------------------
-- PartialSocketClass
-------------------------------------------------------------------------------
data PartialSocketClass =
  PIpSocket (Maybe String) | PUnixSocket PartialDirectoryType
    deriving stock (Show, Eq, Ord, Generic, Typeable)

instance Semigroup PartialSocketClass where
  x <> y = case (x, y) of
    (PIpSocket   a, PIpSocket b) -> PIpSocket $ a <|> b
    (a@(PIpSocket _), PUnixSocket _) -> a
    (PUnixSocket _, a@(PIpSocket _)) -> a
    (PUnixSocket a, PUnixSocket b) -> PUnixSocket $ a <> b

instance Monoid PartialSocketClass where
 mempty = PUnixSocket mempty

initPartialSocketClass :: PartialSocketClass -> IO SocketClass
initPartialSocketClass theClass = case theClass of
  PIpSocket mIp -> pure $ IpSocket $ fromMaybe "127.0.0.1" mIp
  PUnixSocket mFilePath ->
    UnixSocket <$> initDirectoryType "tmp-postgres-socket" mFilePath

shutdownSocketConfig :: SocketClass -> IO ()
shutdownSocketConfig = \case
  IpSocket   {}  -> pure ()
  UnixSocket dir -> shutdownDirectoryType dir
-------------------------------------------------------------------------------
-- PartialPostgresPlan
-------------------------------------------------------------------------------
data PartialPostgresPlan = PartialPostgresPlan
  { partialPostgresPlanProcessConfig :: PartialProcessConfig
  , partialPostgresPlanClientConfig  :: Client.PartialOptions
  }
  deriving stock (Generic)
  deriving Semigroup via GenericSemigroup PartialPostgresPlan
  deriving Monoid    via GenericMonoid PartialPostgresPlan

completePostgresPlan :: PartialPostgresPlan -> Either [String] PostgresPlan
completePostgresPlan PartialPostgresPlan {..} = validationToEither $ do
  postgresPlanClientConfig <-
    eitherToValidation $ addErrorContext "partialPostgresPlanClientConfig: " $
      Client.completeOptions partialPostgresPlanClientConfig
  postgresPlanProcessConfig <-
    eitherToValidation $ addErrorContext "partialPostgresPlanProcessConfig: " $
      completeProcessConfig partialPostgresPlanProcessConfig

  pure PostgresPlan {..}
-------------------------------------------------------------------------------
-- PartialPlan
-------------------------------------------------------------------------------
-- | The monoidial version of 'Plan'. Used to combine overrides with defaults
--   when creating a plan.
data PartialPlan = PartialPlan
  { partialPlanLogger        :: Last Logger
  , partialPlanInitDb        :: Lastoid (Maybe PartialProcessConfig)
  , partialPlanCreateDb      :: Lastoid (Maybe PartialProcessConfig)
  , partialPlanPostgres      :: PartialPostgresPlan
  , partialPlanConfig        :: Lastoid [String]
  , partialPlanDataDirectory :: Last String
  }
  deriving stock (Generic)
  deriving Semigroup via GenericSemigroup PartialPlan
  deriving Monoid    via GenericMonoid PartialPlan

completePlan :: PartialPlan -> Either [String] Plan
completePlan PartialPlan {..} = validationToEither $ do
  planLogger   <- getOption "partialPlanLogger" partialPlanLogger
  planInitDb   <- eitherToValidation $ addErrorContext "partialPlanInitDb: " $
    traverse completeProcessConfig $ getLastoid partialPlanInitDb
  planCreateDb <- eitherToValidation $ addErrorContext "partialPlanCreateDb: " $
    traverse completeProcessConfig $ getLastoid partialPlanCreateDb
  planPostgres <- eitherToValidation $ addErrorContext "partialPlanPostgres: " $
    completePostgresPlan partialPlanPostgres
  let planConfig = unlines $ getLastoid partialPlanConfig
  planDataDirectory <- getOption "partialPlanDataDirectory"
    partialPlanDataDirectory

  pure Plan {..}
-------------------------------------------------------------------------------
-- Resources
-------------------------------------------------------------------------------
data Resources = Resources
  { resourcesPlan    :: Plan
  , resourcesSocket  :: SocketClass
  , resourcesDataDir :: DirectoryType
  }

data Config = Config
  { configPlan    :: PartialPlan
  , configSocket  :: PartialSocketClass
  , configDataDir :: PartialDirectoryType
  , configPort    :: Last (Maybe Int)
  }
  deriving stock (Generic)
  deriving Semigroup via GenericSemigroup Config
  deriving Monoid    via GenericMonoid Config

-- | Create a 'PartialPlan' that sets the command line options of all processes
--   (@initdb@, @postgres@ and @createdb@) using a
toPlan
  :: Int
  -- ^ port
  -> SocketClass
  -- ^ Whether to listen on a IP address or UNIX domain socket
  -> FilePath
  -- ^ The @postgres@ data directory
  -> PartialPlan
toPlan port socketClass dataDirectory = mempty
  { partialPlanConfig = Mappend $ socketClassToConfig socketClass
  , partialPlanDataDirectory = pure dataDirectory
  , partialPlanPostgres = mempty
      { partialPostgresPlanProcessConfig = mempty
          { partialProcessConfigCmdLine = Mappend
              [ "-p", show port
              , "-D", dataDirectory
              ]
          }
      , partialPostgresPlanClientConfig = mempty
          { Client.host = pure $ socketClassToHost socketClass
          , Client.port = pure port
          }
      }
  , partialPlanCreateDb = Mappend $ Just $ mempty
      { partialProcessConfigCmdLine = Mappend $
          socketClassToHostFlag socketClass <>
          ["-p", show port]
      }
  , partialPlanInitDb = Mappend $ Just $ mempty
      { partialProcessConfigCmdLine = Mappend $
          ["--pgdata=" <> dataDirectory]
      }
  }

-- | Create all the temporary resources from a 'Config'. This also combines the 'PartialPlan' from
--   'toPlan' with the @extraConfig@ passed in.
initConfig
  :: Config
  -- ^ @extraConfig@ to 'mappend' after the default config
  -> IO Resources
initConfig Config {..} = evalContT $ do
  port <- lift $ maybe getFreePort pure $ join $ getLast configPort
  resourcesSocket <- ContT $ bracketOnError (initPartialSocketClass configSocket) shutdownSocketConfig
  resourcesDataDir <- ContT $ bracketOnError (initDirectoryType "tmp-postgres-data" configDataDir) shutdownDirectoryType
  let hostAndDirPartial = toPlan port resourcesSocket $ toFilePath resourcesDataDir
  resourcesPlan <- lift $ either (throwIO . CompletePlanFailed) pure $
    completePlan $ hostAndDirPartial <> configPlan
  pure Resources {..}

-- | Free the temporary resources created by 'initConfig'
shutdownResources :: Resources -> IO ()
shutdownResources Resources {..} = do
  shutdownSocketConfig resourcesSocket
  shutdownDirectoryType resourcesDataDir