{-| This module provides types and functions for combining partial
    configs into a complete configs to ultimately make a 'Plan'.

    This module has three classes of types. Types like 'Lastoid' that
    are generic and could live in a module like "base".

    Types like 'PartialProcessConfig' that could be used by any
    library that  needs to combine process options.

    Finally it has types and functions for creating 'Plan's that
    use temporary resources. This is used to create the default
    behavior of 'Database.Postgres.Temp.startWith' and related
    functions.
|-}
module Database.Postgres.Temp.Internal.Partial where
import Database.Postgres.Temp.Internal.Core
import qualified Database.PostgreSQL.Simple.Options as Client
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
import System.IO.Error
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

{- |
'Lastoid' is helper for overriding configuration values.
It's 'Semigroup' instance let's one either combine the
@a@ of two 'Lastoid's using '<>' via the 'Mappend' constructor
or one can wholly replace the value with the last value using the 'Replace'
constructor.
Roughly

 @
   x <> Replace y = Replace y
   Replace x <> Mappend y = Replace (x <> y)
   Mappend x <> Mappend y = Mappend (x <> y)
 @

-}
data Lastoid a = Replace a | Mappend a
  deriving (Show, Eq, Functor)

instance Semigroup a => Semigroup (Lastoid a) where
  x <> y = case (x, y) of
    (_        , r@Replace {}) -> r
    (Replace a, Mappend   b ) -> Replace $ a <> b
    (Mappend a, Mappend   b ) -> Mappend $ a <> b

instance Monoid a => Monoid (Lastoid a) where
  mempty = Mappend mempty

-- | Get the value of a 'Lastoid' regardless if it is a 'Replace' or
--   a 'Mappend'.
getLastoid :: Lastoid a -> a
getLastoid = \case
  Replace a -> a
  Mappend a -> a

-- | A type to help combine command line arguments.
data PartialCommandLineArgs = PartialCommandLineArgs
  { partialCommandLineArgsKeyBased   :: Map String (Maybe String)
  -- ^ Arguments of the form @-h foo@, @--host=foo@ and @--switch@.
  --   The key is `mappend`ed with value so the key should include
  --   the space or equals (as shown in the first two examples
  --   respectively).
  --   The 'Dual' monoid is used so the last key wins.
  , partialCommandLineArgsIndexBased :: Map Int String
  -- ^ Arguments that appear at the end of the key based
  --   arguments.
  --   The 'Dual' monoid is used so the last key wins.
  }
  deriving stock (Generic, Show, Eq)
  deriving Monoid via GenericMonoid PartialCommandLineArgs

instance Semigroup PartialCommandLineArgs where
  x <> y = PartialCommandLineArgs
    { partialCommandLineArgsKeyBased   =
        partialCommandLineArgsKeyBased y <> partialCommandLineArgsKeyBased x
    , partialCommandLineArgsIndexBased =
        partialCommandLineArgsIndexBased y <> partialCommandLineArgsIndexBased x
    }

-- | Take values as long as the index is the successor of the
--   last index.
takeWhileInSequence :: [(Int, a)] -> [a]
takeWhileInSequence ((0, x):xs) = x : go 0 xs where
  go _ [] = []
  go prev ((next, a):rest)
    | prev + 1 == next = a : go next rest
    | otherwise = []
takeWhileInSequence _ = []

-- | This convert the 'PartialCommandLineArgs' to '
completeCommandLineArgs :: PartialCommandLineArgs -> [String]
completeCommandLineArgs PartialCommandLineArgs {..}
  =  map (\(name, mvalue) -> maybe name (name <>) mvalue)
       (Map.toList partialCommandLineArgsKeyBased)
  <> takeWhileInSequence (Map.toList partialCommandLineArgsIndexBased)

-- | The monoidial version of 'ProcessConfig'. Used to combine overrides with
--   defaults when creating a 'ProcessConfig'.
data PartialProcessConfig = PartialProcessConfig
  { partialProcessConfigEnvVars :: Lastoid (Map String String)
  -- ^ A monoid for combine environment variables or replacing them.
  --   for the maps the 'Dual' monoid is used. So the last key wins.
  , partialProcessConfigCmdLine :: Lastoid PartialCommandLineArgs
  -- ^ A monoid for combine command line arguments or replacing them
  , partialProcessConfigStdIn   :: Last Handle
  -- ^ A monoid for configuring the standard input 'Handle'
  , partialProcessConfigStdOut  :: Last Handle
  -- ^ A monoid for configuring the standard output 'Handle'
  , partialProcessConfigStdErr  :: Last Handle
  -- ^ A monoid for configuring the standard error 'Handle'
  }
  deriving stock (Generic)
  deriving Monoid    via GenericMonoid PartialProcessConfig

instance Semigroup PartialProcessConfig where
  x <> y = PartialProcessConfig
    { partialProcessConfigEnvVars = fmap getDual $
        fmap Dual (partialProcessConfigEnvVars x) <>
          fmap Dual (partialProcessConfigEnvVars y)
    , partialProcessConfigCmdLine =
        partialProcessConfigCmdLine x <> partialProcessConfigCmdLine y
    , partialProcessConfigStdIn   =
        partialProcessConfigStdIn x <> partialProcessConfigStdIn y
    , partialProcessConfigStdOut  =
        partialProcessConfigStdOut x <> partialProcessConfigStdOut y
    , partialProcessConfigStdErr  =
        partialProcessConfigStdErr x <> partialProcessConfigStdErr y
    }

-- | The 'standardProcessConfig' sets the handles to 'stdin', 'stdout' and
--   'stderr' and inherits the environment variables from the calling
--   process.
standardProcessConfig :: IO PartialProcessConfig
standardProcessConfig = do
  env <- getEnvironment
  pure mempty
    { partialProcessConfigEnvVars = Replace $ Map.fromList env
    , partialProcessConfigStdIn   = pure stdin
    , partialProcessConfigStdOut  = pure stdout
    , partialProcessConfigStdErr  = pure stderr
    }

-- | A helper to add more info to all the error messages.
addErrorContext :: String -> Either [String] a -> Either [String] a
addErrorContext cxt = either (Left . map (cxt <>)) Right

-- | A helper for creating an error if a 'Last' is not defined.
getOption :: String -> Last a -> Validation [String] a
getOption optionName = \case
    Last (Just x) -> pure x
    Last Nothing  -> Failure ["Missing " ++ optionName ++ " option"]

-- | Turn a 'PartialProcessConfig' into a 'ProcessConfig'. Fails if
--   any values are missing.
completeProcessConfig
  :: PartialProcessConfig -> Either [String] ProcessConfig
completeProcessConfig PartialProcessConfig {..} = validationToEither $ do
  let processConfigEnvVars = Map.toList $ getLastoid partialProcessConfigEnvVars
      processConfigCmdLine = completeCommandLineArgs $
        getLastoid partialProcessConfigCmdLine
  processConfigStdIn  <-
    getOption "partialProcessConfigStdIn" partialProcessConfigStdIn
  processConfigStdOut <-
    getOption "partialProcessConfigStdOut" partialProcessConfigStdOut
  processConfigStdErr <-
    getOption "partialProcessConfigStdErr" partialProcessConfigStdErr

  pure ProcessConfig {..}

-- | A type to track whether a file is temporary and needs to be cleaned up.
data DirectoryType = Permanent FilePath | Temporary FilePath
  deriving(Show, Eq, Ord)

-- | Get the file path of a 'DirectoryType', regardless if it is a
-- 'Permanent' or 'Temporary' type.
toFilePath :: DirectoryType -> FilePath
toFilePath = \case
  Permanent x -> x
  Temporary x -> x

-- | The monoidial version of 'DirectoryType'. Used to combine overrides with
--   defaults when creating a 'DirectoryType'. The monoid instance treats
--   'PTemporary' as 'mempty' and takes the last 'PPermanent' value.
data PartialDirectoryType
  = PPermanent FilePath
  -- ^ A permanent file that should not be generated.
  | PTemporary
  -- ^ A temporary file that needs to generated.
  deriving(Show, Eq, Ord)

instance Semigroup PartialDirectoryType where
  x <> y = case (x, y) of
    (a, PTemporary     ) -> a
    (_, a@PPermanent {}) -> a

instance Monoid PartialDirectoryType where
  mempty = PTemporary

-- | Either create a'Temporary' directory or do nothing to a 'Permanent'
--   one.
initDirectoryType :: String -> PartialDirectoryType -> IO DirectoryType
initDirectoryType pattern = \case
  PTemporary -> Temporary <$> createTempDirectory "/tmp" pattern
  PPermanent x  -> pure $ Permanent x

-- | Either create a temporary directory or do nothing
rmDirIgnoreErrors :: FilePath -> IO ()
rmDirIgnoreErrors mainDir = do
  let ignoreDirIsMissing e
        | isDoesNotExistError e = return ()
        | otherwise = throwIO e
  removeDirectoryRecursive mainDir `catch` ignoreDirIsMissing

-- | Either remove a 'Temporary' directory or do nothing to a 'Permanent'
-- one.
shutdownDirectoryType :: DirectoryType -> IO ()
shutdownDirectoryType = \case
  Permanent _ -> pure ()
  Temporary filePath -> rmDirIgnoreErrors filePath

-- | A type for configuring the listening address of the @postgres@ process.
--   @postgres@ can listen on several types of sockets simulatanously but we
--   don't support that behavior. One can either listen on a IP based socket
--   or a UNIX domain socket.
data SocketClass
  = IpSocket String
  -- ^ IP socket type. The 'String' is either an IP address or
  -- a host that will resolve to an IP address.
  | UnixSocket DirectoryType
  -- ^ UNIX domain socket
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
socketClassToHostFlag :: SocketClass -> [(String, Maybe String)]
socketClassToHostFlag x = [("-h", Just (socketClassToHost x))]

-- | Get the IP address, host name or UNIX domain socket directory
--   as a 'String'
socketClassToHost :: SocketClass -> String
socketClassToHost = \case
  IpSocket ip    -> ip
  UnixSocket dir -> toFilePath dir

-- | The monoidial version of 'SocketClass'. Used to combine overrides with
--   defaults when creating a 'SocketClass'. The monoid instance treats
--   'PUnixSocket mempty' as 'mempty' and combines the
data PartialSocketClass
  = PIpSocket (Last String)
  -- ^ The monoid for combining IP address configuration
  | PUnixSocket PartialDirectoryType
  -- ^ The monoid for combining UNIX socket configuration
    deriving stock (Show, Eq, Ord, Generic, Typeable)

instance Semigroup PartialSocketClass where
  x <> y = case (x, y) of
    (PIpSocket   a, PIpSocket b) -> PIpSocket $ a <> b
    (a@(PIpSocket _), PUnixSocket _) -> a
    (PUnixSocket _, a@(PIpSocket _)) -> a
    (PUnixSocket a, PUnixSocket b) -> PUnixSocket $ a <> b

instance Monoid PartialSocketClass where
 mempty = PUnixSocket mempty

-- | Turn a 'PartialSocketClass' to a 'SocketClass'. If the 'PIpSocket' is
--   'Nothing' default to \"127.0.0.1\". If the is a 'PUnixSocket'
--    optionally create a temporary directory if configured to do so.
initPartialSocketClass :: PartialSocketClass -> IO SocketClass
initPartialSocketClass theClass = case theClass of
  PIpSocket mIp -> pure $ IpSocket $ fromMaybe "127.0.0.1" $
    getLast mIp
  PUnixSocket mFilePath ->
    UnixSocket <$> initDirectoryType "tmp-postgres-socket" mFilePath

-- | Cleanup the UNIX socket temporary directory if one was created.
shutdownSocketConfig :: SocketClass -> IO ()
shutdownSocketConfig = \case
  IpSocket   {}  -> pure ()
  UnixSocket dir -> shutdownDirectoryType dir

-- | PartialPostgresPlan
data PartialPostgresPlan = PartialPostgresPlan
  { partialPostgresPlanProcessConfig :: PartialProcessConfig
  -- ^ Monoid for the @postgres@ ProcessConfig.
  , partialPostgresPlanClientConfig  :: Client.Options
  -- ^ Monoid for the @postgres@ client connection options.
  }
  deriving stock (Generic)
  deriving Semigroup via GenericSemigroup PartialPostgresPlan
  deriving Monoid    via GenericMonoid PartialPostgresPlan

-- | Turn a 'PartialPostgresPlan' into a 'PostgresPlan'. Fails if any
--   values are missing.
completePostgresPlan :: PartialPostgresPlan -> Either [String] PostgresPlan
completePostgresPlan PartialPostgresPlan {..} = validationToEither $ do
  let postgresPlanClientConfig = partialPostgresPlanClientConfig
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

-- | Turn a 'PartialPlan' into a 'Plan'. Fails if any values are missing.
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

-- | 'Resources' holds a description of the temporary folders (if there are any)
--   and includes the final 'Plan' that can be used with 'initPlan'.
--   See 'initConfig' for an example of how to create a 'Resources'.
data Resources = Resources
  { resourcesPlan    :: Plan
  -- ^ Final 'Plan'. See 'initPlan' for information on 'Plan's
  , resourcesSocket  :: SocketClass
  -- ^ The 'SocketClass'. Used to track if a temporary directory was made
  --   as the socket location.
  , resourcesDataDir :: DirectoryType
  -- ^ The data directory. Used to track if a temporary directory was used.
  }

-- | The high level options for overriding default behavior.
data Config = Config
  { configPlan    :: PartialPlan
  -- ^ Extend or replace any of the configuration used to create a final
  --   'Plan'
  , configSocket  :: PartialSocketClass
  -- ^ Override the default 'SocketClass' by setting this.
  , configDataDir :: PartialDirectoryType
  -- ^ Override the default temporary data directory by passing in
  -- 'Permanent DIRECTORY'
  , configPort    :: Last (Maybe Int)
  -- ^ A monoid for using an existing port (via 'Just PORT_NUMBER') or
  -- requesting a free port (via a 'Nothing')
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
          { partialProcessConfigCmdLine = Mappend $ mempty
              { partialCommandLineArgsKeyBased = Map.fromList
                  [ ("-p", Just $ show port)
                  , ("-D", Just dataDirectory)
                  ]
              }
          }
      , partialPostgresPlanClientConfig = mempty
          { Client.host = pure $ socketClassToHost socketClass
          , Client.port = pure port
          }
      }
  , partialPlanCreateDb = Mappend $ Just $ mempty
      { partialProcessConfigCmdLine = Mappend $ mempty
          { partialCommandLineArgsKeyBased = Map.fromList $
              socketClassToHostFlag socketClass <>
              [("-p ", Just $ show port)]
          }
      }
  , partialPlanInitDb = Mappend $ Just $ mempty
      { partialProcessConfigCmdLine = Mappend $ mempty
          { partialCommandLineArgsKeyBased = Map.fromList $
              [("--pgdata=", Just dataDirectory)]
          }

      }
  }


-- | Create all the temporary resources from a 'Config'. This also combines the
-- 'PartialPlan' from 'toPlan' with the @extraConfig@ passed in.
initConfig
  :: Config
  -- ^ @extraConfig@ to 'mappend' after the default config
  -> IO Resources
initConfig Config {..} = evalContT $ do
  port <- lift $ maybe getFreePort pure $ join $ getLast configPort
  resourcesSocket <- ContT $ bracketOnError
    (initPartialSocketClass configSocket) shutdownSocketConfig
  resourcesDataDir <- ContT $ bracketOnError
    (initDirectoryType "tmp-postgres-data" configDataDir) shutdownDirectoryType
  let hostAndDirPartial = toPlan port resourcesSocket $
        toFilePath resourcesDataDir
  resourcesPlan <- lift $ either (throwIO . CompletePlanFailed) pure $
    completePlan $ hostAndDirPartial <> configPlan
  pure Resources {..}

-- | Free the temporary resources created by 'initConfig'
shutdownResources :: Resources -> IO ()
shutdownResources Resources {..} = do
  shutdownSocketConfig resourcesSocket
  shutdownDirectoryType resourcesDataDir
-------------------------------------------------------------------------------
-- Config Generation
-------------------------------------------------------------------------------
-- | Attempt to create a config from a 'Client.Options'. This is useful if
--   want to create a database owned by a specific user you will also log in as
--   among other use cases. It is possible some 'Client.Options' are not
--   supported so don't hesitate to open an issue on github if you find one.
optionsToConfig :: Client.Options -> Config
optionsToConfig opts@Client.Options {..}
  =  ( mempty
       { configPlan = optionsToPlan opts
       , configPort = maybe (Last Nothing) (pure . pure) $ getLast port
       , configSocket = maybe mempty hostToSocketClass $ getLast host
       }
     )
-- | Convert the 'Client.Options' to a 'PartialPlan' that can
--   be connected to with the 'Client.Options'.
optionsToPlan :: Client.Options -> PartialPlan
optionsToPlan opts@Client.Options {..}
  =  maybe mempty dbnameToPlan (getLast dbname)
  <> maybe mempty userToPlan (getLast user)
  <> clientOptionsToPlan opts

-- | Wrap the 'Client.Options' in an appropiate
--   'PartialPostgresPlan'
clientOptionsToPlan :: Client.Options -> PartialPlan
clientOptionsToPlan opts = mempty
  { partialPlanPostgres = mempty
    { partialPostgresPlanClientConfig = opts
    }
  }

-- | Create a 'PartialPlan' given a user
userToPlan :: String -> PartialPlan
userToPlan user = mempty
  { partialPlanCreateDb = Mappend $ Just $ mempty
    { partialProcessConfigCmdLine = Mappend $ mempty
        { partialCommandLineArgsKeyBased = Map.singleton "--username=" $ Just user
        }
    }
  , partialPlanInitDb = Mappend $ Just $ mempty
    { partialProcessConfigCmdLine =  Mappend $ mempty
        { partialCommandLineArgsKeyBased = Map.singleton "--username=" $ Just user
        }
    }
  }

-- | Adds a @createdb@ PartialProcessPlan with the argument
--   as the database name.
dbnameToPlan :: String -> PartialPlan
dbnameToPlan dbName = mempty
  { partialPlanCreateDb = Mappend $ Just $ mempty
    { partialProcessConfigCmdLine = Mappend $ mempty
      { partialCommandLineArgsIndexBased = Map.singleton 0 dbName
      }
    }
  }

-- | Parse a host string as either an UNIX domain socket directory
--   or a domain or IP.
hostToSocketClass :: String -> PartialSocketClass
hostToSocketClass hostOrSocketPath = case hostOrSocketPath of
  '/' : _ -> PUnixSocket $ PPermanent hostOrSocketPath
  _ -> PIpSocket $ pure hostOrSocketPath
