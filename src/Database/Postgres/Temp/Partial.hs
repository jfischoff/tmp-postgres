{-# LANGUAGE ApplicativeDo #-}
module Database.Postgres.Temp.Partial where
import Database.Postgres.Temp.Core
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

-- TODO
-- It is listening on Ip4 and Ip6 when listening on the unix socket

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

getLastoid :: Lastoid a -> a
getLastoid = \case
  Replace a -> a
  Mappend a -> a
-------------------------------------------------------------------------------
-- PartialProcessOptions
-------------------------------------------------------------------------------
data PartialProcessOptions = PartialProcessOptions
  { partialProcessOptionsEnvVars :: Lastoid [(String, String)]
  , partialProcessOptionsCmdLine :: Lastoid [String]
  , partialProcessOptionsStdIn   :: Last Handle
  , partialProcessOptionsStdOut  :: Last Handle
  , partialProcessOptionsStdErr  :: Last Handle
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

getOption :: String -> Last a -> Validation [String] a
getOption optionName = \case
    Last (Just x) -> pure x
    Last Nothing  -> Failure ["Missing " ++ optionName ++ " option"]

completeProcessOptions :: PartialProcessOptions -> Either [String] ProcessOptions
completeProcessOptions PartialProcessOptions {..} = validationToEither $ do
  let processOptionsEnvVars = getLastoid partialProcessOptionsEnvVars
      processOptionsCmdLine = getLastoid partialProcessOptionsCmdLine
  processOptionsStdIn  <- getOption "partialProcessOptionsStdIn" partialProcessOptionsStdIn
  processOptionsStdOut <- getOption "partialProcessOptionsStdOut" partialProcessOptionsStdOut
  processOptionsStdErr <- getOption "partialProcessOptionsStdErr" partialProcessOptionsStdErr

  pure ProcessOptions {..}
-------------------------------------------------------------------------------
-- DirectoryType
-------------------------------------------------------------------------------
data DirectoryType = Permanent FilePath | Temporary FilePath
  deriving(Show, Eq, Ord)

toFilePath :: DirectoryType -> FilePath
toFilePath = \case
  Permanent x -> x
  Temporary x -> x
-------------------------------------------------------------------------------
-- PartialDirectoryType
-------------------------------------------------------------------------------
data PartialDirectoryType = Temp | Perm FilePath
  deriving(Show, Eq, Ord)

instance Semigroup PartialDirectoryType where
  x <> y = case (x, y) of
    (Temp     , a   ) -> a
    (a@Perm {}, _   ) -> a

instance Monoid PartialDirectoryType where
  mempty = Temp

startDirectoryType :: String -> PartialDirectoryType -> IO DirectoryType
startDirectoryType pattern = \case
  Temp -> Temporary <$> createTempDirectory "/tmp" pattern
  Perm x  -> pure $ Permanent x

rmDirIgnoreErrors :: FilePath -> IO ()
rmDirIgnoreErrors mainDir =
  removeDirectoryRecursive mainDir `catch` (\(_ :: IOException) -> return ())

stopDirectoryType :: DirectoryType -> IO ()
stopDirectoryType = \case
  Permanent _ -> pure ()
  Temporary filePath -> rmDirIgnoreErrors filePath
-------------------------------------------------------------------------------
-- SocketClass
-------------------------------------------------------------------------------
data SocketClass = IpSocket String | UnixSocket DirectoryType
  deriving (Show, Eq, Ord, Generic, Typeable)

socketClassToConfig :: SocketClass -> [String]
socketClassToConfig = \case
  IpSocket ip    -> ["listen_addresses = '" <> ip <> "'"]
  UnixSocket dir ->
    [ "listen_addresses = ''"
    , "unix_socket_directories = '" <> toFilePath dir <> "'"
    ]

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
    (PIpSocket   a, PIpSocket b) -> PIpSocket $ a <> b
    (a@(PIpSocket _), PUnixSocket _) -> a
    (PUnixSocket _, a@(PIpSocket _)) -> a
    (PUnixSocket a, PUnixSocket b) -> PUnixSocket $ a <> b

instance Monoid PartialSocketClass where
 mempty = PUnixSocket mempty

isTempSocket :: PartialSocketClass -> Bool
isTempSocket = \case
  PUnixSocket mFilePath -> case mFilePath of
    Temp -> True
    _ -> False
  _ -> False

-- Not sure what the continuation passes style buys us
startPartialSocketClass :: PartialSocketClass -> IO SocketClass
startPartialSocketClass theClass = case theClass of
  PIpSocket mIp -> pure $ IpSocket $ fromMaybe "127.0.0.1" mIp
  PUnixSocket mFilePath ->
    UnixSocket <$> startDirectoryType "tmp-postgres-socket" mFilePath

stopSocketOptions :: SocketClass -> IO ()
stopSocketOptions = \case
  IpSocket   {}  -> pure ()
  UnixSocket dir -> stopDirectoryType dir
-------------------------------------------------------------------------------
-- PartialPostgresPlan
-------------------------------------------------------------------------------
data PartialPostgresPlan = PartialPostgresPlan
  { partialPostgresPlanProcessOptions :: PartialProcessOptions
  , partialPostgresPlanClientOptions  :: Client.PartialOptions
  }
  deriving stock (Generic)
  deriving Semigroup via GenericSemigroup PartialPostgresPlan
  deriving Monoid    via GenericMonoid PartialPostgresPlan

completePostgresPlan :: PartialPostgresPlan -> Either [String] PostgresPlan
completePostgresPlan PartialPostgresPlan {..} = validationToEither $ do
  postgresPlanClientOptions <-
    eitherToValidation $ Client.completeOptions partialPostgresPlanClientOptions
  postgresPlanProcessOptions <-
    eitherToValidation $ completeProcessOptions partialPostgresPlanProcessOptions

  pure PostgresPlan {..}
-------------------------------------------------------------------------------
-- PartialPlan
-------------------------------------------------------------------------------
data PartialPlan = PartialPlan
  { partialPlanLogger        :: Last Logger
  , partialPlanInitDb        :: Lastoid (Maybe PartialProcessOptions)
  , partialPlanCreateDb      :: Lastoid (Maybe PartialProcessOptions)
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
  planInitDb   <- eitherToValidation $
    traverse completeProcessOptions $ getLastoid partialPlanInitDb
  planCreateDb <- eitherToValidation $
    traverse completeProcessOptions $ getLastoid partialPlanCreateDb
  planPostgres <- eitherToValidation $
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

data PartialResources = PartialResources
  { partialResourcesPlan    :: PartialPlan
  , partialResourcesSocket  :: PartialSocketClass
  , partialResourcesDataDir :: PartialDirectoryType
  , partialResourcesPort    :: Last (Maybe Int)
  }
  deriving stock (Generic)
  deriving Semigroup via GenericSemigroup PartialResources
  deriving Monoid    via GenericMonoid PartialResources

-- TODO create db needs to use the entire partial connection
-- options
toPlan :: Int -> SocketClass -> FilePath -> PartialPlan
toPlan port socketClass dataDirectory = mempty
  { partialPlanConfig = Mappend $ socketClassToConfig socketClass
  , partialPlanDataDirectory = pure dataDirectory
  , partialPlanPostgres = mempty
      { partialPostgresPlanProcessOptions = mempty
          { partialProcessOptionsCmdLine = Mappend
              [ "-p", show port
              , "-D", dataDirectory
              ]
          }
      , partialPostgresPlanClientOptions = mempty
          { Client.host = pure $ socketClassToHost socketClass
          , Client.port = pure port
          }
      }
  , partialPlanCreateDb = Mappend $ Just $ mempty
      { partialProcessOptionsCmdLine = Mappend $
          socketClassToHostFlag socketClass <>
          ["-p", show port]
      }
  , partialPlanInitDb = Mappend $ Just $ mempty
      { partialProcessOptionsCmdLine = Mappend $
          ["--pgdata=" <> dataDirectory]
      }
  }

-- This optional creates the temp data dir
-- It also optional makes the data dir
-- It appends those values to the Plan
-- It creates a port if one is not specified
startPartialResources :: PartialResources -> IO Resources
startPartialResources PartialResources {..} = do
  port <- maybe getFreePort pure $ join $ getLast partialResourcesPort
  resourcesSocket      <- startPartialSocketClass partialResourcesSocket
  resourcesDataDir     <- startDirectoryType "tmp-postgres-data" partialResourcesDataDir
  let hostAndDirPartial = toPlan port resourcesSocket $ toFilePath resourcesDataDir
  resourcesPlan <- either (throwIO . CompletePlanFailed) pure $
    completePlan $ partialResourcesPlan <> hostAndDirPartial
  pure Resources {..}

stopResources :: Resources -> IO ()
stopResources Resources {..} = do
  stopSocketOptions resourcesSocket
  stopDirectoryType resourcesDataDir


-- TODO make functions for creating plans from PartialProcessOptions from PartialClientOptions

-- This might go in the other file
-- this is basically startWith
-- startPlanWithResources :: PartialResources -> IO (Resources, PostgresProcess)
-- startPlanWithResources = startPartialResources $ startPlan . resourcesPlan

-- I'm not sure but the partial conn options should probably come but the
-- the start should have a partial options but they should be generated from the function
{-

throwMaybe :: Exception e => e -> Maybe a -> IO a
throwMaybe e = \case
  Nothing -> throwIO e
  Just  x -> pure x

-------------------------------------------------------------------------------
-- CommonOptions life cycle
-------------------------------------------------------------------------------
startPartialCommonOptions
  :: PartialCommonOptions -> (CommonOptions -> IO a) -> IO a
startPartialCommonOptions PartialCommonOptions {..} f = do
  port <- maybe getFreePort pure $ getLast $
    Client.port partialCommonOptionsClientOptions

  let dbName = fromMaybe "test" $ getLast $
        Client.dbname partialCommonOptionsClientOptions

      commonOptionsLogger        = fromMaybe mempty partialCommonOptionsLogger

      initCommon = initializeDirectoryType "tmp-postgres-data"
        partialCommonOptionsDataDir

  bracketOnError initCommon cleanupDirectoryType $ \commonOptionsDataDir ->
      startPartialSocketClass partialCommonOptionsSocketClass $
        \commonOptionsSocketClass -> do
          let host = socketClassToHost commonOptionsSocketClass
              defaultClientOptions = mempty
                { Client.port = pure port
                , Client.dbname = pure dbName
                , Client.host = pure host
                }
          commonOptionsClientOptions <- throwMaybe ClientCompleteOptions .
              either (const Nothing) Just $
                Client.completeOptions $
                  partialCommonOptionsClientOptions <> defaultClientOptions
          f CommonOptions {..}

stopCommonOptions :: CommonOptions -> IO ()
stopCommonOptions CommonOptions {..} = do
  stopSocketOptions commonOptionsSocketClass
  cleanupDirectoryType commonOptionsDataDir


-------------------------------------------------------------------------------
-- PartialPostgresPlan
-------------------------------------------------------------------------------
data PartialPostgresPlan = PartialPostgresPlan
  { partialPostgresPlanConfig  :: Lastoid [String]
  , partialPostgresPlanOptions :: PartialProcessOptions
  } deriving stock (Generic)
    deriving Semigroup via GenericSemigroup PartialPostgresPlan
    deriving Monoid    via GenericMonoid PartialPostgresPlan

defaultConfig :: [String]
defaultConfig =
  [ "shared_buffers = 12MB"
  , "fsync = off"
  , "synchronous_commit = off"
  , "full_page_writes = off"
  , "log_min_duration_statement = 0"
  , "log_connections = on"
  , "log_disconnections = on"
  , "client_min_messages = ERROR"
  ]

defaultPostgresPlan :: CommonOptions -> IO PartialPostgresPlan
defaultPostgresPlan CommonOptions {..} = do
  processOptions <- standardProcessOptions
  pure $ PartialPostgresPlan
    { partialPostgresPlanConfig  = Replace $
        defaultConfig <> listenAddressConfig commonOptionsSocketClass
    , partialPostgresPlanOptions = processOptions
        { partialProcessOptionsName = pure "postgres"
        , partialProcessOptionsCmdLine = Replace
            [ "-D" <> toFilePath commonOptionsDataDir
            , "-p" <> show (fromJust $ PostgresClient.oPort commonOptionsClientOptions)
            ]
        }
    }


completePostgresPlan :: PartialPostgresPlan -> Maybe PostgresPlan
completePostgresPlan PartialPostgresPlan {..} = do
  postgresPlanConfig <- case partialPostgresPlanConfig of
    Mappend _ -> Nothing
    Replace x -> Just $ unlines x

  postgresPlanOptions <- completeProcessOptions partialPostgresPlanOptions

  pure PostgresPlan {..}
-}