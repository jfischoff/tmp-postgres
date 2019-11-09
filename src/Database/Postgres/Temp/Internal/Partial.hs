{-# OPTIONS_HADDOCK prune #-}
{-| This module provides types and functions for combining partial
    configs into a complete configs to ultimately make a 'Plan'.

    This module has two classes of types.

    Types like 'PartialProcessConfig' that could be used by any
    library that  needs to combine process options.

    Finally it has types and functions for creating 'Plan's that
    use temporary resources. This is used to create the default
    behavior of 'Database.Postgres.Temp.startConfig' and related
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
import Control.Applicative.Lift
import Control.Monad.Trans.Cont
import Control.Monad.Trans.Class
import System.IO.Error
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Text.PrettyPrint.ANSI.Leijen hiding ((<$>))

prettyMap :: (Pretty a, Pretty b) => Map a b -> Doc
prettyMap theMap =
  let xs = Map.toList theMap
  in vsep $ map (uncurry prettyKeyPair) xs

-- | The environment variables can be declared to
--   inherit from the running process or they
--   can be specifically added.
data PartialEnvVars = PartialEnvVars
  { partialEnvVarsInherit  :: Last Bool
  , partialEnvVarsSpecific :: Map String String
  }
  deriving stock (Generic, Show, Eq)

instance Semigroup PartialEnvVars where
  x <> y = PartialEnvVars
    { partialEnvVarsInherit  =
        partialEnvVarsInherit x <> partialEnvVarsInherit y
    , partialEnvVarsSpecific =
        partialEnvVarsSpecific y <> partialEnvVarsSpecific x
    }

instance Monoid PartialEnvVars where
  mempty = PartialEnvVars mempty mempty

instance Pretty PartialEnvVars where
  pretty PartialEnvVars {..}
    = text "partialEnvVarsInherit:"
        <+> pretty (getLast partialEnvVarsInherit)
    <> hardline
    <> text "partialEnvVarsSpecific:"
    <> softline
    <> indent 2 (prettyMap partialEnvVarsSpecific)

-- | Combine the current environment
--   (if indicated by 'partialEnvVarsInherit')
--   with 'partialEnvVarsSpecific'
completePartialEnvVars :: [(String, String)] -> PartialEnvVars -> Either [String] [(String, String)]
completePartialEnvVars envs PartialEnvVars {..} = case getLast partialEnvVarsInherit of
  Nothing -> Left ["Inherit not specified"]
  Just x -> Right $ (if x then envs else [])
    <> Map.toList partialEnvVarsSpecific

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

instance Pretty PartialCommandLineArgs where
  pretty p@PartialCommandLineArgs {..}
    = text "partialCommandLineArgsKeyBased:"
    <> softline
    <> indent 2 (prettyMap partialCommandLineArgsKeyBased)
    <> hardline
    <> text "partialCommandLineArgsIndexBased:"
    <> softline
    <> indent 2 (prettyMap partialCommandLineArgsIndexBased)
    <> hardline
    <> text "completed:" <+> text (unwords (completeCommandLineArgs p))

-- Take values as long as the index is the successor of the
-- last index.
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
  { partialProcessConfigEnvVars :: PartialEnvVars
  -- ^ A monoid for combine environment variables or replacing them.
  --   for the maps the 'Dual' monoid is used. So the last key wins.
  , partialProcessConfigCmdLine :: PartialCommandLineArgs
  -- ^ A monoid for combine command line arguments or replacing them
  , partialProcessConfigStdIn   :: Last Handle
  -- ^ A monoid for configuring the standard input 'Handle'
  , partialProcessConfigStdOut  :: Last Handle
  -- ^ A monoid for configuring the standard output 'Handle'
  , partialProcessConfigStdErr  :: Last Handle
  -- ^ A monoid for configuring the standard error 'Handle'
  }
  deriving stock (Generic, Eq, Show)
  deriving Semigroup via GenericSemigroup PartialProcessConfig
  deriving Monoid    via GenericMonoid PartialProcessConfig

prettyHandle :: Handle -> Doc
prettyHandle _ = text "[HANDLE]"

instance Pretty PartialProcessConfig where
  pretty PartialProcessConfig {..}
    = text "partialProcessConfigEnvVars:"
    <> softline
    <> indent 2 (pretty partialProcessConfigEnvVars)
    <> hardline
    <> text "partialProcessConfigCmdLine:"
    <> softline
    <> indent 2 (pretty partialProcessConfigEnvVars)
    <> hardline
    <> text "partialProcessConfigStdIn:" <+>
        pretty (prettyHandle <$> getLast partialProcessConfigStdIn)
    <> hardline
    <> text "partialProcessConfigStdOut:" <+>
        pretty (prettyHandle <$> getLast partialProcessConfigStdOut)
    <> hardline
    <> text "partialProcessConfigStdErr:" <+>
        pretty (prettyHandle <$> getLast partialProcessConfigStdErr)


-- | The 'standardProcessConfig' sets the handles to 'stdin', 'stdout' and
--   'stderr' and inherits the environment variables from the calling
--   process.
standardProcessConfig :: PartialProcessConfig
standardProcessConfig = mempty
  { partialProcessConfigEnvVars = mempty
      { partialEnvVarsInherit = pure True
      }
  , partialProcessConfigStdIn  = pure stdin
  , partialProcessConfigStdOut = pure stdout
  , partialProcessConfigStdErr = pure stderr
  }

-- A helper to add more info to all the error messages.
addErrorContext :: String -> Either [String] a -> Either [String] a
addErrorContext cxt = either (Left . map (cxt <>)) Right

-- A helper for creating an error if a 'Last' is not defined.
getOption :: String -> Last a -> Errors [String] a
getOption optionName = \case
    Last (Just x) -> pure x
    Last Nothing  -> failure ["Missing " ++ optionName ++ " option"]

-- | Turn a 'PartialProcessConfig' into a 'ProcessConfig'. Fails if
--   any values are missing.
completeProcessConfig
  :: [(String, String)] -> PartialProcessConfig -> Either [String] ProcessConfig
completeProcessConfig envs PartialProcessConfig {..} = runErrors $ do
  let processConfigCmdLine = completeCommandLineArgs partialProcessConfigCmdLine
  processConfigEnvVars <- eitherToErrors $
    completePartialEnvVars envs partialProcessConfigEnvVars
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

instance Pretty DirectoryType where
  pretty = \case
    Permanent x -> text "Permanent" <+> pretty x
    Temporary x -> text "Temporary" <+> pretty x

-- | The monoidial version of 'DirectoryType'. Used to combine overrides with
--   defaults when creating a 'DirectoryType'. The monoid instance treats
--   'PTemporary' as 'mempty' and takes the last 'PPermanent' value.
data PartialDirectoryType
  = PPermanent FilePath
  -- ^ A permanent file that should not be generated.
  | PTemporary
  -- ^ A temporary file that needs to generated.
  deriving(Show, Eq, Ord)

instance Pretty PartialDirectoryType where
  pretty = \case
    PPermanent x -> text "Permanent" <+> pretty x
    PTemporary   -> text "Temporary"

instance Semigroup PartialDirectoryType where
  x <> y = case (x, y) of
    (a, PTemporary     ) -> a
    (_, a@PPermanent {}) -> a

instance Monoid PartialDirectoryType where
  mempty = PTemporary

-- | Either create a'Temporary' directory or do nothing to a 'Permanent'
--   one.
setupDirectoryType :: String -> PartialDirectoryType -> IO DirectoryType
setupDirectoryType pattern = \case
  PTemporary -> Temporary <$> createTempDirectory "/tmp" pattern
  PPermanent x  -> pure $ Permanent x

-- Either create a temporary directory or do nothing
rmDirIgnoreErrors :: FilePath -> IO ()
rmDirIgnoreErrors mainDir = do
  let ignoreDirIsMissing e
        | isDoesNotExistError e = return ()
        | otherwise = throwIO e
  removeDirectoryRecursive mainDir `catch` ignoreDirIsMissing

-- | Either remove a 'Temporary' directory or do nothing to a 'Permanent'
-- one.
cleanupDirectoryType :: DirectoryType -> IO ()
cleanupDirectoryType = \case
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

instance Pretty SocketClass where
  pretty = \case
    IpSocket x   -> text "IpSocket:" <+> pretty x
    UnixSocket x -> text "UnixSocket:" <+> pretty x

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

instance Pretty PartialSocketClass where
  pretty = \case
    PIpSocket x -> "IpSocket:" <+> pretty (getLast x)
    PUnixSocket x -> "UnixSocket" <+> pretty x

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
setupPartialSocketClass :: PartialSocketClass -> IO SocketClass
setupPartialSocketClass theClass = case theClass of
  PIpSocket mIp -> pure $ IpSocket $ fromMaybe "127.0.0.1" $
    getLast mIp
  PUnixSocket mFilePath ->
    UnixSocket <$> setupDirectoryType "tmp-postgres-socket" mFilePath

-- | Cleanup the UNIX socket temporary directory if one was created.
cleanupSocketConfig :: SocketClass -> IO ()
cleanupSocketConfig = \case
  IpSocket   {}  -> pure ()
  UnixSocket dir -> cleanupDirectoryType dir

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

instance Pretty PartialPostgresPlan where
  pretty PartialPostgresPlan {..}
    = text "partialPostgresPlanProcessConfig:"
    <> softline
    <> indent 2 (pretty partialPostgresPlanProcessConfig)
    <> hardline
    <> text "partialPostgresPlanClientConfig:"
    <> softline
    <> indent 2 (prettyOptions partialPostgresPlanClientConfig)

-- | Turn a 'PartialPostgresPlan' into a 'PostgresPlan'. Fails if any
--   values are missing.
completePostgresPlan :: [(String, String)] -> PartialPostgresPlan -> Either [String] PostgresPlan
completePostgresPlan envs PartialPostgresPlan {..} = runErrors $ do
  let postgresPlanClientOptions = partialPostgresPlanClientConfig
  postgresPlanProcessConfig <-
    eitherToErrors $ addErrorContext "partialPostgresPlanProcessConfig: " $
      completeProcessConfig envs partialPostgresPlanProcessConfig

  pure PostgresPlan {..}
-------------------------------------------------------------------------------
-- PartialPlan
-------------------------------------------------------------------------------
-- | The monoidial version of 'Plan'. Used to combine overrides with defaults
--   when creating a plan.
data PartialPlan = PartialPlan
  { partialPlanLogger        :: Last Logger
  , partialPlanInitDb        :: Maybe PartialProcessConfig
  , partialPlanCreateDb      :: Maybe PartialProcessConfig
  , partialPlanPostgres      :: PartialPostgresPlan
  , partialPlanConfig        :: [String]
  , partialPlanDataDirectory :: Last String
  }
  deriving stock (Generic)
  deriving Semigroup via GenericSemigroup PartialPlan
  deriving Monoid    via GenericMonoid PartialPlan

instance Pretty PartialPlan where
  pretty PartialPlan {..}
    =  text "partialPlanInitDb:"
    <> softline
    <> indent 2 (pretty partialPlanInitDb)
    <> hardline
    <> text "partialPlanInitDb:"
    <> softline
    <> indent 2 (pretty partialPlanCreateDb)
    <> hardline
    <> text "partialPlanPostgres:"
    <> softline
    <> indent 2 (pretty partialPlanPostgres)
    <> hardline
    <> text "partialPlanConfig:"
    <> softline
    <> indent 2 (vsep $ map text partialPlanConfig)
    <> hardline
    <> text "partialPlanDataDirectory:" <+> pretty (getLast partialPlanDataDirectory)

-- | Turn a 'PartialPlan' into a 'Plan'. Fails if any values are missing.
completePlan :: [(String, String)] -> PartialPlan -> Either [String] Plan
completePlan envs PartialPlan {..} = runErrors $ do
  planLogger   <- getOption "partialPlanLogger" partialPlanLogger
  planInitDb   <- eitherToErrors $ addErrorContext "partialPlanInitDb: " $
    traverse (completeProcessConfig envs) partialPlanInitDb
  planCreateDb <- eitherToErrors $ addErrorContext "partialPlanCreateDb: " $
    traverse (completeProcessConfig envs) partialPlanCreateDb
  planPostgres <- eitherToErrors $ addErrorContext "partialPlanPostgres: " $
    completePostgresPlan envs partialPlanPostgres
  let planConfig = unlines partialPlanConfig
  planDataDirectory <- getOption "partialPlanDataDirectory"
    partialPlanDataDirectory

  pure Plan {..}

-- Returns 'True' if the 'PartialPlan' has a
-- 'Just' 'partialPlanInitDb'
hasInitDb :: PartialPlan -> Bool
hasInitDb PartialPlan {..} = isJust partialPlanInitDb

-- Returns 'True' if the 'PartialPlan' has a
-- 'Just' 'partialPlanCreateDb'
hasCreateDb :: PartialPlan -> Bool
hasCreateDb PartialPlan {..} = isJust partialPlanCreateDb

-- | 'Resources' holds a description of the temporary folders (if there are any)
--   and includes the final 'Plan' that can be used with 'startPlan'.
--   See 'setupConfig' for an example of how to create a 'Resources'.
data Resources = Resources
  { resourcesPlan    :: Plan
  -- ^ Final 'Plan'. See 'startPlan' for information on 'Plan's
  , resourcesSocket  :: SocketClass
  -- ^ The 'SocketClass'. Used to track if a temporary directory was made
  --   as the socket location.
  , resourcesDataDir :: DirectoryType
  -- ^ The data directory. Used to track if a temporary directory was used.
  }

instance Pretty Resources where
  pretty Resources {..}
    =   text "resourcePlan:"
    <>  softline
    <>  indent 2 (pretty resourcesPlan)
    <>  hardline
    <>  text "resourcesSocket:"
    <+> pretty resourcesSocket
    <>  hardline
    <>  text "resourcesDataDir:"
    <+> pretty resourcesDataDir

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

instance Pretty Config where
  pretty Config {..}
    =  text "configPlan:"
    <> softline
    <> pretty configPlan
    <> hardline
    <> text "configSocket:"
    <> softline
    <> pretty configSocket
    <> hardline
    <> text "configDataDir:"
    <> softline
    <> pretty configDataDir
    <> hardline
    <> text "configPort:" <+> pretty (getLast configPort)

-- | Create a 'PartialPlan' that sets the command line options of all processes
--   (@initdb@, @postgres@ and @createdb@) using a
toPlan
  :: Bool
  -- ^ Make @initdb@ options
  -> Bool
  -- ^ Make @createdb@ options
  -> Int
  -- ^ port
  -> SocketClass
  -- ^ Whether to listen on a IP address or UNIX domain socket
  -> FilePath
  -- ^ The @postgres@ data directory
  -> PartialPlan
toPlan makeInitDb makeCreateDb port socketClass dataDirectory = mempty
  { partialPlanConfig = socketClassToConfig socketClass
  , partialPlanDataDirectory = pure dataDirectory
  , partialPlanPostgres = mempty
      { partialPostgresPlanProcessConfig = mempty
          { partialProcessConfigCmdLine = mempty
              { partialCommandLineArgsKeyBased = Map.fromList
                  [ ("-p", Just $ show port)
                  , ("-D", Just dataDirectory)
                  ]
              }
          }
      , partialPostgresPlanClientConfig = mempty
          { Client.host   = pure $ socketClassToHost socketClass
          , Client.port   = pure port
          , Client.dbname = pure "postgres"
          }
      }
  , partialPlanCreateDb = if makeCreateDb
      then pure $ mempty
        { partialProcessConfigCmdLine = mempty
            { partialCommandLineArgsKeyBased = Map.fromList $
                socketClassToHostFlag socketClass <>
                [("-p ", Just $ show port)]
            }
        }
      else Nothing
  , partialPlanInitDb = if makeInitDb
      then pure $ mempty
        { partialProcessConfigCmdLine = mempty
            { partialCommandLineArgsKeyBased = Map.fromList $
                [("--pgdata=", Just dataDirectory)]
            }

        }
      else Nothing
  }


-- | Create all the temporary resources from a 'Config'. This also combines the
-- 'PartialPlan' from 'toPlan' with the @extraConfig@ passed in.
setupConfig
  :: Config
  -- ^ @extraConfig@ to 'mappend' after the default config
  -> IO Resources
setupConfig Config {..} = evalContT $ do
  envs <- lift getEnvironment
  port <- lift $ maybe getFreePort pure $ join $ getLast configPort
  resourcesSocket <- ContT $ bracketOnError
    (setupPartialSocketClass configSocket) cleanupSocketConfig
  resourcesDataDir <- ContT $ bracketOnError
    (setupDirectoryType "tmp-postgres-data" configDataDir) cleanupDirectoryType
  let hostAndDirPartial = toPlan
          (hasInitDb configPlan)
          (hasCreateDb configPlan)
          port
          resourcesSocket
          (toFilePath resourcesDataDir)
      finalPlan = hostAndDirPartial <> configPlan
  resourcesPlan <- lift $
    either (throwIO . CompletePlanFailed (show $ pretty finalPlan)) pure $
      completePlan envs finalPlan
  pure Resources {..}

-- | Free the temporary resources created by 'setupConfig'
cleanupResources :: Resources -> IO ()
cleanupResources Resources {..} = do
  cleanupSocketConfig resourcesSocket
  cleanupDirectoryType resourcesDataDir
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
-- Convert the 'Client.Options' to a 'PartialPlan' that can
-- be connected to with the 'Client.Options'.
optionsToPlan :: Client.Options -> PartialPlan
optionsToPlan opts@Client.Options {..}
  =  maybe mempty dbnameToPlan (getLast dbname)
  <> maybe mempty userToPlan (getLast user)
  <> clientOptionsToPlan opts

-- Wrap the 'Client.Options' in an appropiate
-- 'PartialPostgresPlan'
clientOptionsToPlan :: Client.Options -> PartialPlan
clientOptionsToPlan opts = mempty
  { partialPlanPostgres = mempty
    { partialPostgresPlanClientConfig = opts
    }
  }

-- Create a 'PartialPlan' given a user
userToPlan :: String -> PartialPlan
userToPlan user = mempty
  { partialPlanCreateDb = pure $ mempty
    { partialProcessConfigCmdLine = mempty
        { partialCommandLineArgsKeyBased = Map.singleton "--username=" $ Just user
        }
    }
  , partialPlanInitDb = pure $ mempty
    { partialProcessConfigCmdLine = mempty
        { partialCommandLineArgsKeyBased = Map.singleton "--username=" $ Just user
        }
    }
  }

-- Adds a @createdb@ PartialProcessPlan with the argument
-- as the database name.
dbnameToPlan :: String -> PartialPlan
dbnameToPlan dbName = mempty
  { partialPlanCreateDb = pure $ mempty
    { partialProcessConfigCmdLine = mempty
      { partialCommandLineArgsIndexBased = Map.singleton 0 dbName
      }
    }
  }

-- Parse a host string as either an UNIX domain socket directory
-- or a domain or IP.
hostToSocketClass :: String -> PartialSocketClass
hostToSocketClass hostOrSocketPath = case hostOrSocketPath of
  '/' : _ -> PUnixSocket $ PPermanent hostOrSocketPath
  _ -> PIpSocket $ pure hostOrSocketPath
