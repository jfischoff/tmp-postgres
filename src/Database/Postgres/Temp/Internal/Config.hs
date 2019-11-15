{-# OPTIONS_HADDOCK prune #-}
{-| This module provides types and functions for combining partial
    configs into a complete configs to ultimately make a 'CompletePlan'.

    This module has two classes of types.

    Types like 'ProcessConfig' that could be used by any
    library that  needs to combine process options.

    Finally it has types and functions for creating 'CompletePlan's that
    use temporary resources. This is used to create the default
    behavior of 'Database.Postgres.Temp.startConfig' and related
    functions.
|-}
module Database.Postgres.Temp.Internal.Config where

import Database.Postgres.Temp.Internal.Core

import           Control.Applicative.Lift
import           Control.Exception
import           Control.Monad (join)
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Cont
import qualified Data.Map.Strict as Map
import           Data.Map.Strict (Map)
import           Data.Maybe
import           Data.Monoid
import           Data.Monoid.Generic
import           Data.Typeable
import qualified Database.PostgreSQL.Simple.Options as Client
import           GHC.Generics (Generic)
import           Network.Socket.Free (getFreePort)
import           System.Directory
import           System.Environment
import           System.IO
import           System.IO.Error
import           System.IO.Temp (createTempDirectory)
import           System.IO.Unsafe (unsafePerformIO)
import           Text.PrettyPrint.ANSI.Leijen hiding ((<$>))

prettyMap :: (Pretty a, Pretty b) => Map a b -> Doc
prettyMap theMap =
  let xs = Map.toList theMap
  in vsep $ map (uncurry prettyKeyPair) xs

-- | The environment variables can be declared to
--   inherit from the running process or they
--   can be specifically added.
data EnvironmentVariables = EnvironmentVariables
  { inherit  :: Last Bool
  , specific :: Map String String
  }
  deriving stock (Generic, Show, Eq)

instance Semigroup EnvironmentVariables where
  x <> y = EnvironmentVariables
    { inherit  =
        inherit x <> inherit y
    , specific =
        specific y <> specific x
    }

instance Monoid EnvironmentVariables where
  mempty = EnvironmentVariables mempty mempty

instance Pretty EnvironmentVariables where
  pretty EnvironmentVariables {..}
    = text "inherit:"
        <+> pretty (getLast inherit)
    <> hardline
    <> text "specific:"
    <> softline
    <> indent 2 (prettyMap specific)

-- | Combine the current environment
--   (if indicated by 'inherit')
--   with 'specific'.
completeEnvironmentVariables
  :: [(String, String)]
  -> EnvironmentVariables
  -> Either [String] [(String, String)]
completeEnvironmentVariables envs EnvironmentVariables {..} = case getLast inherit of
  Nothing -> Left ["Inherit not specified"]
  Just x -> Right $ (if x then envs else [])
    <> Map.toList specific

-- | A type to help combine command line Args.
data CommandLineArgs = CommandLineArgs
  { keyBased   :: Map String (Maybe String)
  -- ^ Args of the form @-h foo@, @--host=foo@ and @--switch@.
  --   The key is `mappend`ed with value so the key should include
  --   the space or equals (as shown in the first two examples
  --   respectively).
  --   The 'Dual' monoid is used so the last key wins.
  , indexBased :: Map Int String
  -- ^ Args that appear at the end of the key based
  --   Args.
  --   The 'Dual' monoid is used so the last key wins.
  }
  deriving stock (Generic, Show, Eq)
  deriving Monoid via GenericMonoid CommandLineArgs

instance Semigroup CommandLineArgs where
  x <> y = CommandLineArgs
    { keyBased   =
        keyBased y <> keyBased x
    , indexBased =
        indexBased y <> indexBased x
    }

instance Pretty CommandLineArgs where
  pretty p@CommandLineArgs {..}
    = text "keyBased:"
    <> softline
    <> indent 2 (prettyMap keyBased)
    <> hardline
    <> text "indexBased:"
    <> softline
    <> indent 2 (prettyMap indexBased)
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

-- | This convert the 'CommandLineArgs' to '[String]'.
completeCommandLineArgs :: CommandLineArgs -> [String]
completeCommandLineArgs CommandLineArgs {..}
  =  map (\(name, mvalue) -> maybe name (name <>) mvalue)
       (Map.toList keyBased)
  <> takeWhileInSequence (Map.toList indexBased)

-- | Process configuration
data ProcessConfig = ProcessConfig
  { environmentVariables :: EnvironmentVariables
  -- ^ A monoid for combine environment variables or replacing them.
  --   for the maps the 'Dual' monoid is used. So the last key wins.
  , commandLine :: CommandLineArgs
  -- ^ A monoid for combine command line Args or replacing them.
  , stdIn :: Last Handle
  -- ^ A monoid for configuring the standard input 'Handle'.
  , stdOut :: Last Handle
  -- ^ A monoid for configuring the standard output 'Handle'.
  , stdErr :: Last Handle
  -- ^ A monoid for configuring the standard error 'Handle'.
  }
  deriving stock (Generic, Eq, Show)
  deriving Semigroup via GenericSemigroup ProcessConfig
  deriving Monoid    via GenericMonoid ProcessConfig

prettyHandle :: Handle -> Doc
prettyHandle _ = text "[HANDLE]"

instance Pretty ProcessConfig where
  pretty ProcessConfig {..}
    = text "environmentVariables:"
    <> softline
    <> indent 2 (pretty environmentVariables)
    <> hardline
    <> text "commandLine:"
    <> softline
    <> indent 2 (pretty environmentVariables)
    <> hardline
    <> text "stdIn:" <+>
        pretty (prettyHandle <$> getLast stdIn)
    <> hardline
    <> text "stdOut:" <+>
        pretty (prettyHandle <$> getLast stdOut)
    <> hardline
    <> text "stdErr:" <+>
        pretty (prettyHandle <$> getLast stdErr)


-- | The 'standardProcessConfig' sets the handles to 'stdin', 'stdout' and
--   'stderr' and inherits the environment variables from the calling
--   process.
standardProcessConfig :: ProcessConfig
standardProcessConfig = mempty
  { environmentVariables = mempty
      { inherit = pure True
      }
  , stdIn  = pure stdin
  , stdOut = pure stdout
  , stdErr = pure stderr
  }

devNull :: Handle
devNull = unsafePerformIO (openFile "/dev/null" WriteMode)
{-# NOINLINE devNull #-}

-- | 'silentProcessConfig' sets the handles to @/dev/null@ and
--   inherits the environment variables from the calling process.
silentProcessConfig :: ProcessConfig
silentProcessConfig = mempty
  { environmentVariables = mempty
      { inherit = pure True
      }
  , stdIn  = pure devNull
  , stdOut = pure devNull
  , stdErr = pure devNull
  }

-- A helper to add more info to all the error messages.
addErrorContext :: String -> Either [String] a -> Either [String] a
addErrorContext cxt = either (Left . map (cxt <>)) Right

-- A helper for creating an error if a 'Last' is not defined.
getOption :: String -> Last a -> Errors [String] a
getOption optionName = \case
    Last (Just x) -> pure x
    Last Nothing  -> failure ["Missing " ++ optionName ++ " option"]

-- | Turn a 'ProcessConfig' into a 'ProcessConfig'. Fails if
--   any values are missing.
completeProcessConfig
  :: [(String, String)] -> ProcessConfig -> Either [String] CompleteProcessConfig
completeProcessConfig envs ProcessConfig {..} = runErrors $ do
  let completeProcessConfigCmdLine = completeCommandLineArgs commandLine
  completeProcessConfigEnvVars <- eitherToErrors $
    completeEnvironmentVariables envs environmentVariables
  completeProcessConfigStdIn  <-
    getOption "stdIn" stdIn
  completeProcessConfigStdOut <-
    getOption "stdOut" stdOut
  completeProcessConfigStdErr <-
    getOption "stdErr" stdErr

  pure CompleteProcessConfig {..}

-- | A type to track whether a file is temporary and needs to be cleaned up.
data CompleteDirectoryType = CPermanent FilePath | CTemporary FilePath
  deriving(Show, Eq, Ord)

-- | Get the file path of a 'CompleteDirectoryType', regardless if it is a
-- 'CPermanent' or 'CTemporary' type.
toFilePath :: CompleteDirectoryType -> FilePath
toFilePath = \case
  CPermanent x -> x
  CTemporary x -> x

instance Pretty CompleteDirectoryType where
  pretty = \case
    CPermanent x -> text "CPermanent" <+> pretty x
    CTemporary x -> text "CTemporary" <+> pretty x

makePermanent :: CompleteDirectoryType -> CompleteDirectoryType
makePermanent = \case
  CTemporary x -> CPermanent x
  x -> x

-- | Used to specify a 'Temporary' folder that is automatically
--   cleaned up or a 'Permanent' folder which is not
--   automatically cleaned up.
data DirectoryType
  = Permanent FilePath
  -- ^ A permanent file that should not be generated.
  | Temporary
  -- ^ A temporary file that needs to generated.
  deriving(Show, Eq, Ord)

instance Pretty DirectoryType where
  pretty = \case
    Permanent x -> text "CPermanent" <+> pretty x
    Temporary   -> text "CTemporary"

-- | Takes the last 'Permanent' value.
instance Semigroup DirectoryType where
  x <> y = case (x, y) of
    (a, Temporary     ) -> a
    (_, a@Permanent {}) -> a

-- | 'Temporary' as 'mempty'
instance Monoid DirectoryType where
  mempty = Temporary

-- | Either create a'CTemporary' directory or do nothing to a 'CPermanent'
--   one.
setupDirectoryType
  :: String
  -- ^ Temporary directory configuration
  -> String
  -- ^ Directory pattern
  -> DirectoryType
  -> IO CompleteDirectoryType
setupDirectoryType tempDir pat = \case
  Temporary -> CTemporary <$> createTempDirectory tempDir pat
  Permanent x  -> pure $ CPermanent x

-- Remove a temporary directory and ignore errors
-- about it not being there.
rmDirIgnoreErrors :: FilePath -> IO ()
rmDirIgnoreErrors mainDir = do
  let ignoreDirIsMissing e
        | isDoesNotExistError e = return ()
        | otherwise = throwIO e
  removeDirectoryRecursive mainDir `catch` ignoreDirIsMissing

-- | Either remove a 'CTemporary' directory or do nothing to a 'CPermanent'
-- one.
cleanupDirectoryType :: CompleteDirectoryType -> IO ()
cleanupDirectoryType = \case
  CPermanent _ -> pure ()
  CTemporary filePath -> rmDirIgnoreErrors filePath

-- | A type for configuring the listening address of the @postgres@ process.
--   @postgres@ can listen on several types of sockets simulatanously but we
--   don't support that behavior. One can either listen on a IP based socket
--   or a UNIX domain socket.
data CompleteSocketClass
  = CIpSocket String
  -- ^ IP socket type. The 'String' is either an IP address or
  -- a host that will resolve to an IP address.
  | CUnixSocket CompleteDirectoryType
  -- ^ UNIX domain socket
  deriving (Show, Eq, Ord, Generic, Typeable)

instance Pretty CompleteSocketClass where
  pretty = \case
    CIpSocket x   -> text "CIpSocket:" <+> pretty x
    CUnixSocket x -> text "CUnixSocket:" <+> pretty x

-- | Create the extra config lines for listening based on the 'CompleteSocketClass'.
socketClassToConfig :: CompleteSocketClass -> [String]
socketClassToConfig = \case
  CIpSocket ip    -> ["listen_addresses = '" <> ip <> "'"]
  CUnixSocket dir ->
    [ "listen_addresses = ''"
    , "unix_socket_directories = '" <> toFilePath dir <> "'"
    ]

-- | Many processes require a \"host\" flag. We can generate one from the
--   'CompleteSocketClass'.
socketClassToHostFlag :: CompleteSocketClass -> [(String, Maybe String)]
socketClassToHostFlag x = [("-h", Just (socketClassToHost x))]

-- | Get the IP address, host name or UNIX domain socket directory
--   as a 'String'.
socketClassToHost :: CompleteSocketClass -> String
socketClassToHost = \case
  CIpSocket ip    -> ip
  CUnixSocket dir -> toFilePath dir

-- | 'SocketClass' is used to specify how @postgres@ should listen for connections
--   The two main options are a `IpSocket` which takes a hostname or IP address.
--   if not is given the default it "127.0.0.1". Alternatively one can
--   specify 'UnixSocket' for a UNIX domain socket. If a directory is
--   specified the socket will live in that folder. Otherwise a
--   temporary folder will get created for the socket.
data SocketClass
  = IpSocket (Last String)
  -- ^ The monoid for combining IP address configuration.
  | UnixSocket DirectoryType
  -- ^ The monoid for combining UNIX socket configuration.
    deriving stock (Show, Eq, Ord, Generic, Typeable)

instance Pretty SocketClass where
  pretty = \case
    IpSocket x -> "CIpSocket:" <+> pretty (getLast x)
    UnixSocket x -> "CUnixSocket" <+> pretty x

-- | Last 'IpSocket' wins. 'UnixSocket' 'DirectoryType' as
--   'mappend'ed together.
instance Semigroup SocketClass where
  x <> y = case (x, y) of
    (IpSocket   a, IpSocket b) -> IpSocket $ a <> b
    (a@(IpSocket _), UnixSocket _) -> a
    (UnixSocket _, a@(IpSocket _)) -> a
    (UnixSocket a, UnixSocket b) -> UnixSocket $ a <> b

-- | Treats 'UnixSocket' 'mempty' as 'mempty'.
instance Monoid SocketClass where
 mempty = UnixSocket mempty

-- | Turn a 'SocketClass' to a 'CompleteSocketClass'. If the 'IpSocket' is
--   'Nothing' default to \"127.0.0.1\". If the is a 'UnixSocket'
--    optionally create a temporary directory if configured to do so.
setupSocketClass
  :: String
  -- ^ Temporary directory.
  -> SocketClass
  -- ^ The type of socket.
  -> IO CompleteSocketClass
setupSocketClass tempDir theClass = case theClass of
  IpSocket mIp -> pure $ CIpSocket $ fromMaybe "127.0.0.1" $
    getLast mIp
  UnixSocket mFilePath ->
    CUnixSocket <$> setupDirectoryType tempDir "tmp-postgres-socket" mFilePath

-- | Cleanup the UNIX socket temporary directory if one was created.
cleanupSocketConfig :: CompleteSocketClass -> IO ()
cleanupSocketConfig = \case
  CIpSocket   {}  -> pure ()
  CUnixSocket dir -> cleanupDirectoryType dir

-- | @postgres@ process config and corresponding client connection
--   'Client.Options'.
data PostgresPlan = PostgresPlan
  { postgresConfig :: ProcessConfig
  -- ^ Monoid for the @postgres@ ProcessConfig.
  , connectionOptions :: Client.Options
  -- ^ Monoid for the @postgres@ client connection options.
  }
  deriving stock (Generic)
  deriving Semigroup via GenericSemigroup PostgresPlan
  deriving Monoid    via GenericMonoid PostgresPlan

instance Pretty PostgresPlan where
  pretty PostgresPlan {..}
    = text "postgresConfig:"
    <> softline
    <> indent 2 (pretty postgresConfig)
    <> hardline
    <> text "connectionOptions:"
    <> softline
    <> indent 2 (prettyOptions connectionOptions)

-- | Turn a 'PostgresPlan' into a 'CompletePostgresPlan'. Fails if any
--   values are missing.
completePostgresPlan :: [(String, String)] -> PostgresPlan -> Either [String] CompletePostgresPlan
completePostgresPlan envs PostgresPlan {..} = runErrors $ do
  let completePostgresPlanClientOptions = connectionOptions
  completePostgresPlanProcessConfig <-
    eitherToErrors $ addErrorContext "postgresConfig: " $
      completeProcessConfig envs postgresConfig

  pure CompletePostgresPlan {..}
-------------------------------------------------------------------------------
-- Plan
-------------------------------------------------------------------------------
-- | Describe how to run @initdb@, @createdb@ and @postgres@
data Plan = Plan
  { logger :: Last Logger
  , initDbConfig :: Maybe ProcessConfig
  , createDbConfig :: Maybe ProcessConfig
  , postgresPlan :: PostgresPlan
  , postgresConfigFile :: [String]
  , dataDirectoryString :: Last String
  , connectionTimeout :: Last Int
  -- ^ Max time to spend attempting to connection to @postgres@.
  --   Time is in microseconds.
  }
  deriving stock (Generic)
  deriving Semigroup via GenericSemigroup Plan
  deriving Monoid    via GenericMonoid Plan

instance Pretty Plan where
  pretty Plan {..}
    =  text "initDbConfig:"
    <> softline
    <> indent 2 (pretty initDbConfig)
    <> hardline
    <> text "initDbConfig:"
    <> softline
    <> indent 2 (pretty createDbConfig)
    <> hardline
    <> text "postgresPlan:"
    <> softline
    <> indent 2 (pretty postgresPlan)
    <> hardline
    <> text "postgresConfigFile:"
    <> softline
    <> indent 2 (vsep $ map text postgresConfigFile)
    <> hardline
    <> text "dataDirectoryString:" <+> pretty (getLast dataDirectoryString)
    <> hardline
    <> text "connectionTimeout:" <+> pretty (getLast connectionTimeout)

-- | Turn a 'Plan' into a 'CompletePlan'. Fails if any values are missing.
completePlan :: [(String, String)] -> Plan -> Either [String] CompletePlan
completePlan envs Plan {..} = runErrors $ do
  completePlanLogger   <- getOption "logger" logger
  completePlanInitDb   <- eitherToErrors $ addErrorContext "initDbConfig: " $
    traverse (completeProcessConfig envs) initDbConfig
  completePlanCreateDb <- eitherToErrors $ addErrorContext "createDbConfig: " $
    traverse (completeProcessConfig envs) createDbConfig
  completePlanPostgres <- eitherToErrors $ addErrorContext "postgresPlan: " $
    completePostgresPlan envs postgresPlan
  let completePlanConfig = unlines postgresConfigFile
  completePlanDataDirectory <- getOption "dataDirectoryString"
    dataDirectoryString
  completePlanConnectionTimeout <- getOption "connectionTimeout"
    connectionTimeout

  pure CompletePlan {..}

-- Returns 'True' if the 'Plan' has a
-- 'Just' 'initDbConfig'.
hasInitDb :: Plan -> Bool
hasInitDb Plan {..} = isJust initDbConfig

-- Returns 'True' if the 'Plan' has a
-- 'Just' 'createDbConfig'.
hasCreateDb :: Plan -> Bool
hasCreateDb Plan {..} = isJust createDbConfig

-- | The high level options for overriding default behavior.
data Config = Config
  { plan    :: Plan
  -- ^ Extend or replace any of the configuration used to create a final
  --   'CompletePlan'.
  , socketClass  :: SocketClass
  -- ^ Override the default 'CompleteSocketClass' by setting this.
  , dataDirectory :: DirectoryType
  -- ^ Override the default temporary data directory by passing in
  -- 'Permanent' @DIRECTORY@.
  , port    :: Last (Maybe Int)
  -- ^ A monoid for using an existing port (via 'Just' @PORT_NUMBER@) or
  -- requesting a free port (via a 'Nothing').
  , temporaryDirectory :: Last FilePath
  -- ^ The directory used to create other temporary directories. Defaults
  --   to @/tmp@.
  }
  deriving stock (Generic)
  deriving Semigroup via GenericSemigroup Config
  deriving Monoid    via GenericMonoid Config

instance Pretty Config where
  pretty Config {..}
    =  text "plan:"
    <> softline
    <> pretty plan
    <> hardline
    <> text "socketClass:"
    <> softline
    <> pretty socketClass
    <> hardline
    <> text "dataDirectory:"
    <> softline
    <> pretty dataDirectory
    <> hardline
    <> text "port:" <+> pretty (getLast port)
    <> hardline
    <> text "dataDirectory:"
    <> softline
    <> pretty (getLast temporaryDirectory)

-- | Create a 'Plan' that sets the command line options of all processes
--   (@initdb@, @postgres@ and @createdb@). This the @generated@ plan
--   that is combined with the @extra@ plan from 'startConfig'.
toPlan
  :: Bool
  -- ^ Make @initdb@ options
  -> Bool
  -- ^ Make @createdb@ options
  -> Int
  -- ^ port
  -> CompleteSocketClass
  -- ^ Whether to listen on a IP address or UNIX domain socket
  -> FilePath
  -- ^ The @postgres@ data directory
  -> Plan
toPlan makeInitDb makeCreateDb port socketClass dataDirectoryString = mempty
  { postgresConfigFile = socketClassToConfig socketClass
  , dataDirectoryString = pure dataDirectoryString
  , connectionTimeout = pure (60 * 1000000) -- 1 minute
  , logger = pure print
  , postgresPlan = mempty
      { postgresConfig = standardProcessConfig
          { commandLine = mempty
              { keyBased = Map.fromList
                  [ ("-p", Just $ show port)
                  , ("-D", Just dataDirectoryString)
                  ]
              }
          }
      , connectionOptions = mempty
          { Client.host   = pure $ socketClassToHost socketClass
          , Client.port   = pure port
          , Client.dbname = pure "postgres"
          }
      }
  , createDbConfig = if makeCreateDb
      then pure $ standardProcessConfig
        { commandLine = mempty
            { keyBased = Map.fromList $
                socketClassToHostFlag socketClass <>
                [("-p ", Just $ show port)]
            }
        }
      else Nothing
  , initDbConfig = if makeInitDb
      then pure $ standardProcessConfig
        { commandLine = mempty
            { keyBased = Map.fromList
                [("--pgdata=", Just dataDirectoryString)]
            }

        }
      else Nothing
  }


-- | Create all the temporary resources from a 'Config'. This also combines the
-- 'Plan' from 'toPlan' with the @extra@ 'Config' passed in.
setupConfig
  :: Config
  -- ^ @extra@ 'Config' to 'mappend' after the @generated@ 'Config'.
  -> IO Resources
setupConfig Config {..} = evalContT $ do
  envs <- lift getEnvironment
  thePort <- lift $ maybe getFreePort pure $ join $ getLast port
  let resourcesTemporaryDir = fromMaybe "/tmp" $ getLast temporaryDirectory
  resourcesSocket <- ContT $ bracketOnError
    (setupSocketClass resourcesTemporaryDir socketClass) cleanupSocketConfig
  resourcesDataDir <- ContT $ bracketOnError
    (setupDirectoryType resourcesTemporaryDir "tmp-postgres-data" dataDirectory) cleanupDirectoryType
  let hostAndDir = toPlan
        (hasInitDb plan)
        (hasCreateDb plan)
        thePort
        resourcesSocket
        (toFilePath resourcesDataDir)
      finalPlan = hostAndDir <> plan
  resourcesPlan <- lift $
    either (throwIO . CompletePlanFailed (show $ pretty finalPlan)) pure $
      completePlan envs finalPlan
  pure Resources {..}

-- | Free the temporary resources created by 'setupConfig'.
cleanupConfig :: Resources -> IO ()
cleanupConfig Resources {..} = do
  cleanupSocketConfig resourcesSocket
  cleanupDirectoryType resourcesDataDir

-- | 'Resources' holds a description of the temporary folders (if there are any)
--   and includes the final 'CompletePlan' that can be used with 'startPlan'.
--   See 'setupConfig' for an example of how to create a 'Resources'.
data Resources = Resources
  { resourcesPlan    :: CompletePlan
  -- ^ Final 'CompletePlan'. See 'startPlan' for information on 'CompletePlan's.
  , resourcesSocket  :: CompleteSocketClass
  -- ^ The 'CompleteSocketClass'. Used to track if a temporary directory was made
  --   as the socket location.
  , resourcesDataDir :: CompleteDirectoryType
  -- ^ The data directory. Used to track if a temporary directory was used.
  , resourcesTemporaryDir :: FilePath
  -- ^ The directory where other temporary directories are created.
  --   Usually @/tmp.
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

-- | Make the 'resourcesDataDir' 'CPermanent' so it will not
--   get cleaned up.
makeResourcesDataDirPermanent :: Resources -> Resources
makeResourcesDataDirPermanent r = r
  { resourcesDataDir = makePermanent $ resourcesDataDir r
  }
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
       { plan = optionsToPlan opts
       , port = maybe (Last Nothing) (pure . pure) $ getLast port
       , socketClass = maybe mempty hostToSocketClass $ getLast host
       }
     )
-- Convert the 'Client.Options' to a 'Plan' that can
-- be connected to with the 'Client.Options'.
optionsToPlan :: Client.Options -> Plan
optionsToPlan opts@Client.Options {..}
  =  maybe mempty dbnameToPlan (getLast dbname)
  <> maybe mempty userToPlan (getLast user)
  <> clientOptionsToPlan opts

-- Wrap the 'Client.Options' in an appropiate
-- 'PostgresPlan'.
clientOptionsToPlan :: Client.Options -> Plan
clientOptionsToPlan opts = mempty
  { postgresPlan = mempty
    { connectionOptions = opts
    }
  }

-- Create a 'Plan' given a user.
userToPlan :: String -> Plan
userToPlan user = mempty
  { createDbConfig = pure $ mempty
    { commandLine = mempty
        { keyBased = Map.singleton "--username=" $ Just user
        }
    }
  , initDbConfig = pure $ mempty
    { commandLine = mempty
        { keyBased = Map.singleton "--username=" $ Just user
        }
    }
  }

-- Adds a @createdb@ ProcessPlan with the argument
-- as the database name.
dbnameToPlan :: String -> Plan
dbnameToPlan dbName = mempty
  { createDbConfig = pure $ mempty
    { commandLine = mempty
      { indexBased = Map.singleton 0 dbName
      }
    }
  }

-- Parse a host string as either an UNIX domain socket directory
-- or a domain or IP.
hostToSocketClass :: String -> SocketClass
hostToSocketClass hostOrSocketPath = case hostOrSocketPath of
  '/' : _ -> UnixSocket $ Permanent hostOrSocketPath
  _ -> IpSocket $ pure hostOrSocketPath

-------------------------------------------------------------------------------
-- Lenses
-- Most this code was generated with microlens-th
-------------------------------------------------------------------------------
-- | Local Lens alias.
type Lens s t a b = forall f. Functor f => (a -> f b) -> s -> f t
-- | Local Lens' alias.
type Lens' s a = Lens s s a a

-- | Lens for 'inherit'
inheritL :: Lens' EnvironmentVariables (Last Bool)
inheritL f_aj5e (EnvironmentVariables x_aj5f x_aj5g)
  = fmap (`EnvironmentVariables` x_aj5g)
      (f_aj5e x_aj5f)
{-# INLINE inheritL #-}

-- | Lens for 'specific'.
specificL :: Lens' EnvironmentVariables (Map String String)
specificL f_aj5i (EnvironmentVariables x_aj5j x_aj5k)
  = fmap (EnvironmentVariables x_aj5j)
      (f_aj5i x_aj5k)
{-# INLINE specificL #-}

-- | Lens for 'commandLine'.
commandLineL ::
  Lens' ProcessConfig CommandLineArgs
commandLineL
  f_allv
  (ProcessConfig x_allw x_allx x_ally x_allz x_allA)
  = fmap
       (\ y_allB
          -> ProcessConfig x_allw y_allB x_ally x_allz
               x_allA)
      (f_allv x_allx)
{-# INLINE commandLineL #-}

-- | Lens for 'environmentVariables'.
environmentVariablesL ::
  Lens' ProcessConfig EnvironmentVariables
environmentVariablesL
  f_allC
  (ProcessConfig x_allD x_allE x_allF x_allG x_allH)
  = fmap
       (\ y_allI
          -> ProcessConfig y_allI x_allE x_allF x_allG
               x_allH)
      (f_allC x_allD)
{-# INLINE environmentVariablesL #-}

-- | Lens for 'stdErr'
stdErrL ::
  Lens' ProcessConfig (Last Handle)
stdErrL
  f_allJ
  (ProcessConfig x_allK x_allL x_allM x_allN x_allO)
  = fmap
       (ProcessConfig x_allK x_allL x_allM x_allN)
      (f_allJ x_allO)
{-# INLINE stdErrL #-}

-- | Lens for 'stdIn'.
stdInL ::
  Lens' ProcessConfig (Last Handle)
stdInL
  f_allQ
  (ProcessConfig x_allR x_allS x_allT x_allU x_allV)
  = fmap
       (\ y_allW
          -> ProcessConfig x_allR x_allS y_allW x_allU
               x_allV)
      (f_allQ x_allT)
{-# INLINE stdInL #-}

-- | Lens for 'stdOut'.
stdOutL ::
  Lens' ProcessConfig (Last Handle)
stdOutL
  f_allX
  (ProcessConfig x_allY x_allZ x_alm0 x_alm1 x_alm2)
  = fmap
       (\ y_alm3
          -> ProcessConfig x_allY x_allZ x_alm0 y_alm3
               x_alm2)
      (f_allX x_alm1)
{-# INLINE stdOutL #-}

-- | Lens for 'connectionOptions'.
connectionOptionsL ::
  Lens' PostgresPlan Client.Options
connectionOptionsL
  f_am1y
  (PostgresPlan x_am1z x_am1A)
  = fmap (PostgresPlan x_am1z)
      (f_am1y x_am1A)
{-# INLINE connectionOptionsL #-}

-- | Lens for 'postgresConfig'.
postgresConfigL ::
  Lens' PostgresPlan ProcessConfig
postgresConfigL
  f_am1C
  (PostgresPlan x_am1D x_am1E)
  = fmap (`PostgresPlan` x_am1E)
      (f_am1C x_am1D)
{-# INLINE postgresConfigL #-}

-- | Lens for 'postgresConfigFile'.
postgresConfigFileL :: Lens' Plan [String]
postgresConfigFileL f (plan@Plan{..})
  = fmap (\x -> plan { postgresConfigFile = x })
      (f postgresConfigFile)
{-# INLINE postgresConfigFileL #-}

-- | Lens for 'createDbConfig'.
createDbConfigL ::
  Lens' Plan (Maybe ProcessConfig)
createDbConfigL f (plan@Plan{..})
  = fmap (\x -> plan { createDbConfig = x })
      (f createDbConfig)
{-# INLINE createDbConfigL #-}

-- | Lens for 'dataDirectoryString'.
dataDirectoryStringL :: Lens' Plan (Last String)
dataDirectoryStringL f (plan@Plan{..})
  = fmap (\x -> plan { dataDirectoryString = x })
      (f dataDirectoryString)
{-# INLINE dataDirectoryStringL #-}

-- | Lens for 'initDbConfig'.
initDbConfigL :: Lens' Plan (Maybe ProcessConfig)
initDbConfigL f (plan@Plan{..})
  = fmap (\x -> plan { initDbConfig = x })
      (f initDbConfig)
{-# INLINE initDbConfigL #-}

-- | Lens for 'logger'.
loggerL :: Lens' Plan (Last Logger)
loggerL f (plan@Plan{..})
  = fmap (\x -> plan { logger = x })
      (f logger)
{-# INLINE loggerL #-}

-- | Lens for 'postgresPlan'
postgresPlanL :: Lens' Plan PostgresPlan
postgresPlanL f (plan@Plan{..})
  = fmap (\x -> plan { postgresPlan = x })
      (f postgresPlan)
{-# INLINE postgresPlanL #-}

-- | Lens for 'connectionTimeout'
connectionTimeoutL :: Lens' Plan (Last Int)
connectionTimeoutL f (plan@Plan{..})
  = fmap (\x -> plan { connectionTimeout = x })
      (f connectionTimeout)
{-# INLINE connectionTimeoutL #-}

-- | Lens for 'resourcesDataDir'.
resourcesDataDirL :: Lens' Resources CompleteDirectoryType
resourcesDataDirL f (resources@Resources {..})
  = fmap (\x -> resources { resourcesDataDir = x })
      (f resourcesDataDir)
{-# INLINE resourcesDataDirL #-}

-- | Lens for 'resourcesPlan'.
resourcesPlanL :: Lens' Resources CompletePlan
resourcesPlanL f (resources@Resources {..})
  = fmap (\x -> resources { resourcesPlan = x })
      (f resourcesPlan)
{-# INLINE resourcesPlanL #-}

-- | Lens for 'resourcesSocket'.
resourcesSocketL :: Lens' Resources CompleteSocketClass
resourcesSocketL f (resources@Resources {..})
  = fmap (\x -> resources { resourcesSocket = x })
      (f resourcesSocket)
{-# INLINE resourcesSocketL #-}

-- | Lens for 'dataDirectory'.
dataDirectoryL :: Lens' Config DirectoryType
dataDirectoryL f (config@Config{..})
  = fmap (\ x -> config { dataDirectory = x } )
      (f dataDirectory)
{-# INLINE dataDirectoryL #-}

-- | Lens for 'plan'.
planL :: Lens' Config Plan
planL f (config@Config{..})
  = fmap (\ x -> config { plan = x } )
      (f plan)
{-# INLINE planL #-}

-- | Lens for 'port'.
portL :: Lens' Config (Last (Maybe Int))
portL f (config@Config{..})
  = fmap (\ x -> config { port = x } )
      (f port)
{-# INLINE portL #-}

-- | Lens for 'socketClass'.
socketClassL :: Lens' Config SocketClass
socketClassL f (config@Config{..})
  = fmap (\ x -> config { socketClass = x } )
      (f socketClass)
{-# INLINE socketClassL #-}

-- | Lens for 'socketClass'.
temporaryDirectoryL :: Lens' Config (Last FilePath)
temporaryDirectoryL f (config@Config{..})
  = fmap (\ x -> config { temporaryDirectory = x } )
      (f temporaryDirectory)
{-# INLINE temporaryDirectoryL #-}

-- | Lens for 'indexBased'.
indexBasedL ::
  Lens' CommandLineArgs (Map Int String)
indexBasedL
  f_amNr
  (CommandLineArgs x_amNs x_amNt)
  = fmap (CommandLineArgs x_amNs)
      (f_amNr x_amNt)
{-# INLINE indexBasedL #-}

-- | Lens for 'keyBased'.
keyBasedL ::
  Lens' CommandLineArgs (Map String (Maybe String))
keyBasedL
  f_amNv
  (CommandLineArgs x_amNw x_amNx)
  = fmap (`CommandLineArgs` x_amNx)
      (f_amNv x_amNw)
{-# INLINE keyBasedL #-}
