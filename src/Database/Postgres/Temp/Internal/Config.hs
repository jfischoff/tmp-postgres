{-# OPTIONS_HADDOCK prune #-}
{-| This module provides types and functions for combining partial
    configs into a complete configs to ultimately make a 'Plan'.

    This module has two classes of types.

    Types like 'ProcessConfig' that could be used by any
    library that  needs to combine process options.

    Finally it has types and functions for creating 'Plan's that
    use temporary resources. This is used to create the default
    behavior of 'Database.Postgres.Temp.startConfig' and related
    functions.
|-}
module Database.Postgres.Temp.Internal.Config where

import Database.Postgres.Temp.Internal.Core

import           Control.Applicative.Lift
import           Control.DeepSeq
import           Control.Exception
import           Control.Monad (join)
import           Crypto.Hash.SHA1 (hash)
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Cont
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Base64.URL as Base64
import qualified Data.ByteString.Lazy.Char8 as LBSC
import           Data.Char
import qualified Data.Map.Strict as Map
import           Data.Map.Strict (Map)
import           Data.Maybe
import           Data.Monoid
import           Data.Monoid.Generic
import           Data.List
import           Data.Traversable
import qualified Database.PostgreSQL.Simple.Options as Client
import           GHC.Generics (Generic)
import           Network.Socket.Free (getFreePort)
import           Prettyprinter
import           System.Directory
import           System.Environment
import           System.IO (Handle, openFile, IOMode(..))
import qualified System.IO as IO
import           System.IO.Error
import           System.IO.Temp (createTempDirectory)
import           System.IO.Unsafe (unsafePerformIO)
import qualified System.Process.Typed as P
import           Control.Applicative

{-|

'Accum' is a monoid.

It's '<>' behavior is analogous to 1 and 0 with '*'. Think of 'DontCare'
as 1 and 'Zlich' as 0.

The behavior of 'Merge' is like 'Just's.

@since 1.17.0.0
-}
data Accum a = DontCare | Zlich | Merge a
  deriving stock (Show, Eq, Ord, Functor)

instance Applicative Accum where
  pure = Merge
  af <*> ax = case (af, ax) of
    (Merge f, Merge x) -> Merge $ f x

    (DontCare, _) -> DontCare
    (_, DontCare) -> DontCare

    (Zlich, _) -> Zlich
    (_, Zlich) -> Zlich

instance Semigroup a => Semigroup (Accum a) where
  x <> y = case (x, y) of
    (DontCare,         b) -> b
    (a       , DontCare ) -> a

    (Zlich   , _    ) -> Zlich
    (_       , Zlich) -> Zlich

    (Merge a, Merge b) -> Merge $ a <> b

getAccum :: Accum a -> Maybe a
getAccum = \case
  Merge a -> Just a
  _ -> Nothing

instance Monoid a => Monoid (Accum a) where
  mempty = DontCare

prettyMap :: (Pretty a, Pretty b) => Map a b -> Doc ann
prettyMap theMap =
  let xs = Map.toList theMap
  in vsep $ map (uncurry prettyKeyPair) xs

-- | The environment variables can be declared to
--   inherit from the running process or they
--   can be specifically added.
--
--   @since 1.12.0.0
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
    = "inherit:"
        <+> pretty (getLast inherit)
    <> hardline
    <> "specific:"
    <> softline
    <> indent 2 (prettyMap specific)

-- | Combine the current environment
--   (if indicated by 'inherit')
--   with 'specific'.
--
--   @since 1.12.0.0
completeEnvironmentVariables
  :: [(String, String)]
  -> EnvironmentVariables
  -> Either [String] [(String, String)]
completeEnvironmentVariables envs EnvironmentVariables {..} = case getLast inherit of
  Nothing -> Left ["Inherit not specified"]
  Just x -> Right $ (if x then envs else [])
    <> Map.toList specific

-- | A type to help combine command line Args.
--
--   @since 1.12.0.0
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
    = "keyBased:"
    <> softline
    <> indent 2 (prettyMap keyBased)
    <> hardline
    <> "indexBased:"
    <> softline
    <> indent 2 (prettyMap indexBased)
    <> hardline
    <> "completed:" <+> pretty (unwords (completeCommandLineArgs p))

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
--
--   @since 1.12.0.0
completeCommandLineArgs :: CommandLineArgs -> [String]
completeCommandLineArgs CommandLineArgs {..}
  =  map (\(name, mvalue) -> maybe name (name <>) mvalue)
       (Map.toList keyBased)
  <> takeWhileInSequence (Map.toList indexBased)

-- | Process configuration
--
--   @since 1.12.0.0
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
  , createGroup :: Any
  -- ^ A monoid for combining boolean create process group flag.
  --   The first 'True' value wins.
  }
  deriving stock (Generic, Eq, Show)
  deriving Semigroup via GenericSemigroup ProcessConfig
  deriving Monoid    via GenericMonoid ProcessConfig

instance Pretty ProcessConfig where
  pretty ProcessConfig {..}
    = "environmentVariables:"
    <> softline
    <> indent 2 (pretty environmentVariables)
    <> hardline
    <> "commandLine:"
    <> softline
    <> indent 2 (pretty environmentVariables)
    <> hardline
    <> "stdIn:" <+>
        maybe mempty prettyHandle (getLast stdIn)
    <> hardline
    <> "stdOut:" <+>
        maybe mempty prettyHandle (getLast stdOut)
    <> hardline
    <> "stdErr:" <+>
        maybe mempty prettyHandle (getLast stdErr)
    <> hardline
    <> "createGroup:" <+>
        pretty (getAny createGroup)


-- | The 'standardProcessConfig' sets the handles to 'stdin', 'stdout' and
--   'stderr' and inherits the environment variables from the calling
--   process.
--
--   @since 1.12.0.0
standardProcessConfig :: ProcessConfig
standardProcessConfig = mempty
  { environmentVariables = mempty
      { inherit = pure True
      }
  , stdIn  = pure IO.stdin
  , stdOut = pure IO.stdout
  , stdErr = pure IO.stderr
  }

-- | A global reference to @\/dev\/null@ 'Handle'.
--
--   @since 1.12.0.0
devNull :: Handle
devNull = unsafePerformIO (openFile "/dev/null" WriteMode)
{-# NOINLINE devNull #-}

-- | 'silentProcessConfig' sets the handles to @\/dev\/null@ and
--   inherits the environment variables from the calling process.
--
--   @since 1.12.0.0
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
--
--   @since 1.12.0.0
completeProcessConfig
  :: [(String, String)] -> ProcessConfig -> Either [String] CompleteProcessConfig
completeProcessConfig envs ProcessConfig {..} = runErrors $ do
  let completeProcessConfigCmdLine = completeCommandLineArgs commandLine
      completeProcessConfigCreateGroup = getAny createGroup
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
--
--   @since 1.12.0.0
data CompleteDirectoryType = CPermanent FilePath | CTemporary FilePath
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData)

-- | Get the file path of a 'CompleteDirectoryType', regardless if it is a
-- 'CPermanent' or 'CTemporary' type.
--
--   @since 1.12.0.0
toFilePath :: CompleteDirectoryType -> FilePath
toFilePath = \case
  CPermanent x -> x
  CTemporary x -> x

instance Pretty CompleteDirectoryType where
  pretty = \case
    CPermanent x -> "CPermanent" <+> pretty x
    CTemporary x -> "CTemporary" <+> pretty x

makePermanent :: CompleteDirectoryType -> CompleteDirectoryType
makePermanent = \case
  CTemporary x -> CPermanent x
  x -> x

-- | Used to specify a 'Temporary' folder that is automatically
--   cleaned up or a 'Permanent' folder which is not
--   automatically cleaned up.
--
--   @since 1.12.0.0
data DirectoryType
  = Permanent FilePath
  -- ^ A permanent file that should not be generated.
  | Temporary
  -- ^ A temporary file that needs to generated.
  deriving(Show, Eq, Ord)

instance Pretty DirectoryType where
  pretty = \case
    Permanent x -> "Permanent" <+> pretty x
    Temporary   -> "Temporary"

-- | Takes the last 'Permanent' value.
instance Semigroup DirectoryType where
  x <> y = case (x, y) of
    (a, Temporary     ) -> a
    (_, a@Permanent {}) -> a

-- | 'Temporary' as 'mempty'
instance Monoid DirectoryType where
  mempty = Temporary

fixPath :: FilePath -> IO FilePath
fixPath x = case x of
  '~':rest -> do
    homeDir <- getHomeDirectory
    pure $ homeDir <> "/" <> rest
  xs -> pure xs

-- | Either create a'CTemporary' directory or do create the directory
--   if it does not exist to a 'CPermanent'
--   one.
--
--   @since 1.29.0.0
setupDirectoryType
  :: String
  -- ^ Temporary directory configuration
  -> String
  -- ^ Directory pattern
  -> DirectoryType
  -> IO CompleteDirectoryType
setupDirectoryType tempDir pat dirType = case dirType of
  Temporary -> CTemporary <$> createTempDirectory tempDir pat
  Permanent x  -> CPermanent <$> fixPath x

-- Remove a temporary directory and ignore errors
-- about it not being there.
rmDirIgnoreErrors :: FilePath -> IO ()
rmDirIgnoreErrors mainDir = do
  let ignoreDirIsMissing e
        | isDoesNotExistError e = return ()
        | otherwise = throwIO e

  -- Files are continued to be written after the delete starts. This
  -- seems to fix it. #122
  -- TODO come up with a better way to deal with this. Probably
  -- need to lock the directories recursively before deleting.
  handle ignoreDirIsMissing $
    try (removeDirectoryRecursive mainDir) >>= \case
    Left (_ :: IOError) -> try (removeDirectoryRecursive mainDir) >>= \case
      Left (_ :: IOError) -> removeDirectoryRecursive mainDir
      Right _ -> pure ()
    Right _ -> pure ()

-- | Either remove a 'CTemporary' directory or do nothing to a 'CPermanent'
-- one.
cleanupDirectoryType :: CompleteDirectoryType -> IO ()
cleanupDirectoryType = \case
  CPermanent _ -> pure ()
  CTemporary filePath -> rmDirIgnoreErrors filePath

-- | Turn a 'Config' into a 'CompletePostgresPlan'. Fails if any
--   values are missing.
completePostgresPlan :: [(String, String)] -> Config -> Either [String] CompletePostgresPlan
completePostgresPlan envs Config {..} = runErrors $ do
  let completePostgresPlanClientOptions = connectionOptions
  completePostgresPlanProcessConfig <-
    eitherToErrors $ addErrorContext "postgresConfig: " $
      completeProcessConfig envs postgresConfig

  pure CompletePostgresPlan {..}

flattenConfig :: [(String, String)] -> String
flattenConfig = unlines . map (\(x, y) -> x <> "=" <> y) .
  Map.toList . Map.fromList

-- | Turn a 'Config' into a 'Plan'. Fails if any values are missing.
completePlan :: [(String, String)] -> String -> Config -> Either [String] Plan
completePlan envs dataDirectoryString config@Config {..} = do
  (   completePlanLogger
    , completePlanCreateDb
    , completePlanPostgres
    , completePlanDataDirectory
    , completePlanConnectionTimeout
    ) <- runErrors
         $ (,,,,)
        <$> getOption "logger" logger
        <*> eitherToErrors (addErrorContext "createDbConfig: " $
              traverse (completeProcessConfig envs) $ getAccum createDbConfig)
        <*> eitherToErrors (addErrorContext "postgresPlan: "
              (completePostgresPlan envs config))
        <*> pure dataDirectoryString
        <*> getOption "connectionTimeout" connectionTimeout

  let completePlanConfig = flattenConfig postgresConfigFile
      completePlanCopy = completeCopyDirectory completePlanDataDirectory <$>
        join (getLast copyConfig)

  completePlanInitDb <- addErrorContext "initDbConfig: " $ completeInitDb envs dataDirectoryString config

  pure Plan {..}

removeDataDirectory :: ProcessConfig -> ProcessConfig
removeDataDirectory processConfig@ProcessConfig{..} =
  let CommandLineArgs {..} = commandLine
      newCommandLine = commandLine
        { keyBased = Map.delete "--pgdata=" $ Map.delete "-D" keyBased
        }
      newEnvironmentVariables = environmentVariables
        { specific = Map.delete "PGDATA" $ specific environmentVariables
        }

  in processConfig
      { commandLine = newCommandLine
      , environmentVariables = newEnvironmentVariables
      }

-- This needs the complete data directory like completeCopyDirectory
completeInitDb :: [(String, String)] -> FilePath -> Config -> Either [String] (Maybe (Either CompleteProcessConfig InitDbCachePlan))
completeInitDb envs theDataDirectory Config {..} = for (getAccum initDbConfig) $ \theInitDbConfig -> case join $ getLast initDbCache of
  Nothing -> Left <$> completeProcessConfig envs theInitDbConfig
  Just (cow, cacheDirectory) -> do
    let
      clearedConfig = removeDataDirectory theInitDbConfig


    completeClearedPlan <- completeProcessConfig envs clearedConfig
    let
      cachePath = makeCachePath cacheDirectory theCommandLine
      cachePlanDataDirectory = cachePath <> "/data"
      cachePlanCopy = CompleteCopyDirectoryCommand
        { copyDirectoryCommandSrc = cachePlanDataDirectory
        , copyDirectoryCommandDst = theDataDirectory
        , copyDirectoryCommandCow = cow
        }
      theCommandLine = makeInitDbCommandLine completeClearedPlan

      modifiedConfig = clearedConfig <> mempty
        { commandLine = mempty
            { keyBased = Map.fromList [("--pgdata=", Just cachePlanDataDirectory)]
            }
        }

    cachePlanInitDb <- completeProcessConfig envs modifiedConfig
    pure $ Right InitDbCachePlan {..}

-- Returns 'True' if the 'Config' has a
-- 'Just' 'initDbConfig'.
hasInitDb :: Config -> Bool
hasInitDb Config {..} = isJust $ getAccum initDbConfig

-- Returns 'True' if the 'Config' has a
-- 'Just' 'createDbConfig'.
hasCreateDb :: Config -> Bool
hasCreateDb Config {..} = isJust $ getAccum createDbConfig

-- | The high level options for overriding default behavior.
--
--   @since 1.22.0.0
data Config = Config
  { logger :: Last Logger
  -- ^ Internal 'Event' logger.
  , initDbConfig :: Accum ProcessConfig
  -- ^ Monoid for accumulating @initdb@ configuration.
  , copyConfig :: Last (Maybe CopyDirectoryCommand)
  -- ^ An optional data directory copy command.
  , createDbConfig :: Accum ProcessConfig
  -- ^ Monoid for accumulating @createdb@ configuration.
  , postgresConfig :: ProcessConfig
  -- ^ The @postgres@ process configuration.
  , connectionOptions :: Client.Options
  -- ^ The additional client connection options.
  , postgresConfigFile :: [(String, String)]
  -- ^ The @postgresql.conf@ configuration file.
  , connectionTimeout :: Last Int
  -- ^ The amount of microseconds to attempt to connect
  --   to @postgres@ before throwing 'ConnectionTimedOut'
  , socketDirectory  :: DirectoryType
  -- ^ Override the default temporary UNIX socket directory by setting this.
  , dataDirectory :: DirectoryType
  -- ^ Override the default temporary data directory by passing in
  -- 'Permanent' @DIRECTORY@.
  , port    :: Last (Maybe Int)
  -- ^ A monoid for using an existing port (via 'Just' @PORT_NUMBER@) or
  -- requesting a free port (via a 'Nothing').
  , temporaryDirectory :: Last FilePath
  -- ^ The directory used to create other temporary directories. Defaults
  --   to @/tmp@.
  , initDbCache :: Last (Maybe (Bool, FilePath))
  }
  deriving stock (Generic)
  deriving Semigroup via GenericSemigroup Config
  deriving Monoid    via GenericMonoid Config

instance Pretty Config where
  pretty Config {..}
    =  "socketDirectory:"
    <> softline
    <> pretty socketDirectory
    <> hardline
    <> "dataDirectory:"
    <> softline
    <> pretty dataDirectory
    <> hardline
    <> "port:" <+> pretty (getLast port)
    <> hardline
    <> "temporaryDirectory:"
    <> softline
    <> pretty (getLast temporaryDirectory)
    <> hardline
    <> "initDbCache:" <+> pretty (getLast initDbCache)
    <> hardline
    <> "initDbConfig:"
    <> softline
    <> indent 2 (pretty $ getAccum initDbConfig)
    <> hardline
    <> "initDbConfig:"
    <> softline
    <> indent 2 (pretty $ getAccum createDbConfig)
    <> "copyConfig:"
    <> softline
    <> indent 2 (pretty (getLast copyConfig))
    <> hardline
    <> "postgresConfig:"
    <> softline
    <> indent 2 (pretty postgresConfig)
    <> hardline
    <> "connectionOptions:"
    <> softline
    <> indent 2 (prettyOptions connectionOptions)
    <> hardline
    <> "postgresConfigFile:"
    <> softline
    <> indent 2 (vsep $ map (\(x, y) -> pretty x <> "=" <> pretty y) postgresConfigFile)
    <> hardline
    <> "connectionTimeout:" <+> pretty (getLast connectionTimeout)

socketDirectoryToConfig :: FilePath -> [(String, String)]
socketDirectoryToConfig dir =
    [ ("listen_addresses", "'127.0.0.1,::1'")
    , ("unix_socket_directories", "'" <> dir <> "'")
    ]

-------------------------------------------------------------------------------
-- Caching
-------------------------------------------------------------------------------
{-|
Copy command used to create a data directory. If @initdb@ used to create the
data directory directly this is not needed.

If 'destinationDirectory' is Nothing then the 'dataDirectory'
(which might be generated) is used.

@since 1.16.0.0
-}
data CopyDirectoryCommand = CopyDirectoryCommand
  { sourceDirectory :: FilePath
  , destinationDirectory :: Maybe FilePath
  , useCopyOnWrite :: Bool
  } deriving (Show, Eq, Ord)

instance Pretty CopyDirectoryCommand where
  pretty CopyDirectoryCommand {..}
    =  "sourceDirectory:"
    <> softline
    <> indent 2 (pretty sourceDirectory)
    <> hardline
    <> "destinationDirectory:"
    <> softline
    <> indent 2 (pretty destinationDirectory)
    <> hardline
    <> "useCopyOnWrite:"
    <+> pretty useCopyOnWrite

completeCopyDirectory
  :: FilePath
  -> CopyDirectoryCommand
  -> CompleteCopyDirectoryCommand
completeCopyDirectory theDataDirectory CopyDirectoryCommand {..} =
  CompleteCopyDirectoryCommand
    { copyDirectoryCommandSrc = sourceDirectory
    , copyDirectoryCommandDst = fromMaybe theDataDirectory destinationDirectory
    , copyDirectoryCommandCow = useCopyOnWrite
    }

getInitDbVersion :: String
getInitDbVersion = unsafePerformIO $ do
    (stdout, _stderr) <- P.readProcess_ (P.proc "initdb" ["--version"])
        `catch` (throwIO . InitDbFailed)
    let stdoutString = LBSC.unpack stdout
        theLastPart = last $ words stdoutString
        versionPart = takeWhile (\x -> isDigit x || x == '.' || x == '-') theLastPart
        humanReadable =
            if last versionPart == '.'
            then init versionPart
            else versionPart
    pure $ humanReadable <> take 8 (makeArgumentHash stdoutString)
{-# NOINLINE getInitDbVersion #-}

makeCommandLine :: String -> CompleteProcessConfig -> String
makeCommandLine command CompleteProcessConfig {..} =
  let envs = unwords $ map (\(x, y) -> x <> "=" <> y)
             $ filter ((`elem` envsToKeep) . fst) completeProcessConfigEnvVars
      args = unwords completeProcessConfigCmdLine
  in envs <> " " <> command <> args

makeInitDbCommandLine :: CompleteProcessConfig -> String
makeInitDbCommandLine = makeCommandLine "initdb"

makeArgumentHash :: String -> String
makeArgumentHash = BSC.unpack . Base64.encode . hash . BSC.pack

makeCachePath :: FilePath -> String -> String
makeCachePath cacheFolder cmdLine =
  let
    version = getInitDbVersion
    theHash = makeArgumentHash cmdLine
  in cacheFolder <> "/" <> version <> "/" <> theHash

envsToKeep :: [String]
envsToKeep =
  [ "PGHOST"
  , "PGHOSTADDR"
  , "PGPORT"
  , "PGDATABASE"
  , "PGUSER"
  , "PGPASSWORD"
  , "PGPASSFILE"
  , "PGSERVICE"
  , "PGSERVICEFILE"
  , "PGOPTIONS"
  , "PGAPPNAME"
  , "PGSSLMODE"
  , "PGREQUIRESSL"
  , "PGSSLCOMPRESSION"
  , "PGSSLCERT"
  , "PGSSLKEY"
  , "PGSSLROOTCERT"
  , "PGSSLCRL"
  , "PGREQUIREPEER"
  , "PGKRBSRVNAME"
  , "PGGSSLIB"
  , "PGCONNECT_TIMEOUT"
  , "PGCLIENTENCODING"
  , "PGTARGETSESSIONATTRS"
  , "PGDATESTYLE"
  , "PGTZ"
  , "PGGEQO"
  , "PGSYSCONFDIR"
  , "PGLOCALEDIR"
  ]

splitDataDirectory :: CompleteProcessConfig -> (Maybe String, CompleteProcessConfig)
splitDataDirectory old =
  let isDataDirectoryFlag xs = "-D" `isPrefixOf` xs || "--pgdata=" `isPrefixOf` xs
      (dataDirectoryArgs, otherArgs) =
        partition isDataDirectoryFlag $ completeProcessConfigCmdLine old

      firstDataDirectoryArg = flip fmap (listToMaybe dataDirectoryArgs) $ \case
        '-':'D':' ':theDir -> theDir
        '-':'D':theDir -> theDir
        '-':'-':'p':'g':'d':'a':'t':'a':'=':theDir -> theDir
        _ -> error "splitDataDirectory not possible"

      filteredEnvs = filter (("PGDATA" /=) . fst) $
        completeProcessConfigEnvVars old

      clearedConfig = old
        { completeProcessConfigCmdLine = otherArgs
        , completeProcessConfigEnvVars = filteredEnvs
        }

  in (firstDataDirectoryArg, clearedConfig)

addDataDirectory :: String -> CompleteProcessConfig -> CompleteProcessConfig
addDataDirectory theDataDirectory x = x
  { completeProcessConfigCmdLine =
      ("--pgdata=" <> theDataDirectory) : completeProcessConfigCmdLine x
  }

-- | Create a 'Config' that sets the command line options of all processes
--   (@initdb@, @postgres@ and @createdb@). This the @generated@ plan
--   that is combined with the @extra@ plan from
--   'Database.Postgres.Temp.startConfig'.
toPlan
  :: Bool
  -- ^ Make @initdb@ options.
  -> Bool
  -- ^ Make @createdb@ options.
  -> Int
  -- ^ The port.
  -> FilePath
  -- ^ Socket directory.
  -> FilePath
  -- ^ The @postgres@ data directory.
  -> Config
toPlan _makeInitDb makeCreateDb port socketDirectory dataDirectoryString = mempty
  { postgresConfigFile = socketDirectoryToConfig socketDirectory
  , connectionTimeout = pure (60 * 1000000) -- 1 minute
  , logger = pure $ const $ pure ()
  , postgresConfig = silentProcessConfig
    { commandLine = mempty
        { keyBased = Map.fromList
            [ ("-p", Just $ show port)
            , ("-D", Just dataDirectoryString)
            ]
        }
    }
  , connectionOptions = mempty
    { Client.host   = pure socketDirectory
    , Client.port   = pure port
    , Client.dbname = pure "postgres"
    }
  , createDbConfig = if makeCreateDb
      then pure silentProcessConfig
        { commandLine = mempty
            { keyBased = Map.fromList
                [ ("-h", Just socketDirectory)
                , ("-p ", Just $ show port)
                ]
            }
        }
      else mempty

  , initDbConfig = pure silentProcessConfig
        { commandLine = mempty
            { keyBased = Map.fromList
                [("--pgdata=", Just dataDirectoryString)]
            }
        }
  , copyConfig = pure Nothing
  }

-- | Create all the temporary resources from a 'Config'. This also combines the
-- 'Config' from 'toPlan' with the @extra@ 'Config' passed in.
setupConfig
  :: Config
  -- ^ @extra@ 'Config' to 'mappend' after the @generated@ 'Config'.
  -> IO Resources
setupConfig config@Config {..} = evalContT $ do
  envs <- lift getEnvironment
  thePort <- lift $ maybe getFreePort pure $ join $ getLast port
  tmpEnv <- lift $ lookupEnv "TMP"
  tmpDirEnv <- lift $ lookupEnv "TMPDIR"
  let defaultTemp = fromMaybe "/tmp" $ tmpEnv <|> tmpDirEnv
      resourcesTemporaryDir = fromMaybe defaultTemp $ getLast temporaryDirectory
      resourcesInitDbCache = join $ getLast initDbCache
  resourcesSocketDirectory <- ContT $ bracketOnError
    (setupDirectoryType resourcesTemporaryDir "tmp-postgres-socket" socketDirectory) cleanupDirectoryType
  resourcesDataDir <- ContT $ bracketOnError
    (setupDirectoryType resourcesTemporaryDir "tmp-postgres-data" dataDirectory) cleanupDirectoryType
  let hostAndDir = toPlan
        (hasInitDb config)
        (hasCreateDb config)
        thePort
        (toFilePath resourcesSocketDirectory)
        (toFilePath resourcesDataDir)
      finalPlan = hostAndDir <> config
  resourcesPlan <- lift $
    either (throwIO . PlanFailed (show $ pretty finalPlan)) pure $
      completePlan envs (toFilePath resourcesDataDir) finalPlan
--  resourcesPlan <- lift $ maybe (pure uncachedPlan) (uncurry $ cachePlan uncachedPlan) resourcesInitDbCache
  pure Resources {..}

-- | Free the temporary resources created by 'setupConfig'.
cleanupConfig :: Resources -> IO ()
cleanupConfig Resources {..} = do
  cleanupDirectoryType resourcesSocketDirectory
  cleanupDirectoryType resourcesDataDir

-- | Display a 'Config'.
--
--   @since 1.12.0.0
prettyPrintConfig :: Config -> String
prettyPrintConfig = show . pretty

-- | 'Resources' holds a description of the temporary folders (if there are any)
--   and includes the final 'Plan' that can be used with 'startPlan'.
--   See 'setupConfig' for an example of how to create a 'Resources'.
--
--   @since 1.12.0.0
data Resources = Resources
  { resourcesPlan    :: Plan
  -- ^ Final 'Plan'. See 'startPlan' for information on 'Plan's.
  , resourcesSocketDirectory :: CompleteDirectoryType
  -- ^ The used to potentially cleanup the temporary unix socket directory.
  , resourcesDataDir :: CompleteDirectoryType
  -- ^ The data directory. Used to track if a temporary directory was used.
  , resourcesTemporaryDir :: FilePath
  -- ^ The directory where other temporary directories are created.
  --   Usually @/tmp.
  , resourcesInitDbCache :: Maybe (Bool, FilePath)
  }

instance Pretty Resources where
  pretty Resources {..}
    =   "resourcePlan:"
    <>  softline
    <>  indent 2 (pretty resourcesPlan)
    <>  hardline
    <>  "resourcesSocket:"
    <+> pretty resourcesSocketDirectory
    <>  hardline
    <>  "resourcesDataDir:"
    <+> pretty resourcesDataDir

-- | Make the 'resourcesDataDir' 'CPermanent' so it will not
--   get cleaned up.
--
--   @since 1.12.0.0
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
       { port = maybe (Last Nothing) (pure . pure) $ getLast port
       , socketDirectory = maybe mempty hostToSocketClass $ getLast host
       }
     ) <> optionsToPlan opts
-- Convert the 'Client.Options' to a 'Config' that can
-- be connected to with the 'Client.Options'.
optionsToPlan :: Client.Options -> Config
optionsToPlan opts@Client.Options {..}
  =  maybe mempty (dbnameToPlan (getLast user) (getLast password)) (getLast dbname)
  <> maybe mempty userToPlan (getLast user)
  <> maybe mempty passwordToPlan (getLast password)
  <> clientOptionsToPlan opts

-- Wrap the 'Client.Options' in an appropiate
-- 'PostgresPlan'.
clientOptionsToPlan :: Client.Options -> Config
clientOptionsToPlan opts = mempty
  { connectionOptions = opts
  }

-- Create a 'Config' given a user.
userToPlan :: String -> Config
userToPlan user = mempty
  { initDbConfig = pure mempty
    { commandLine = mempty
        { keyBased = Map.singleton "--username=" $ Just user
        }
    }
  }

-- Adds a @createdb@ ProcessPlan with the argument
-- as the database name.
-- It does nothing if the db names are "template1" or
-- "postgres"
dbnameToPlan :: Maybe String -> Maybe String -> String -> Config
dbnameToPlan muser mpassword dbName
  | dbName == "template1" || dbName == "postgres" = mempty
  | otherwise = mempty
    { createDbConfig = pure mempty
      { commandLine = mempty
        { indexBased = Map.singleton 0 dbName
        , keyBased = maybe mempty (Map.singleton "--username=" . Just) muser
        }
      , environmentVariables = mempty
        { specific = maybe mempty (Map.singleton "PGPASSWORD") mpassword
        }
      }
    }

-- Adds the 'PGPASSWORD' to both @initdb@ and @createdb@
passwordToPlan :: String -> Config
passwordToPlan password = mempty
  { initDbConfig = pure mempty
    { environmentVariables = mempty
      { specific = Map.singleton "PGPASSWORD" password
      }
    }
  }

-- Parse a host string as either an UNIX domain socket directory
-- or a domain or IP.
hostToSocketClass :: String -> DirectoryType
hostToSocketClass hostOrSocketPath = case hostOrSocketPath of
  '/' : _ -> Permanent hostOrSocketPath
  _ -> Temporary
