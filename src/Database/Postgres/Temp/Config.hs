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
module Database.Postgres.Temp.Config
  ( Config (..)
  , prettyPrintConfig
    -- *** 'Config' Lenses
  , planL
  , socketClassL
  , dataDirectoryL
  , portL
  , connectionTimeoutL
  -- ** 'Plan'
  , Plan (..)
  -- *** 'Plan' lenses
  , postgresConfigFileL
  , createDbConfigL
  , dataDirectoryStringL
  , initDbConfigL
  , loggerL
  , postgresPlanL
  -- ** 'PostgresPlan'
  , PostgresPlan (..)
  -- *** 'PostgresPlan' lenses
  , connectionOptionsL
  , postgresConfigL
  -- ** 'ProcessConfig'
  , ProcessConfig (..)
  -- *** 'ProcessConfig' Lenses
  , commandLineL
  , environmentVariablesL
  , stdErrL
  , stdInL
  , stdOutL
  -- ** 'EnvironmentVariables'
  , EnvironmentVariables (..)
  -- *** 'EnvironmentVariables' Lenses
  , inheritL
  , specificL
  -- ** 'CommandLineArgs'
  , CommandLineArgs (..)
  -- *** 'CommandLineArgs' Lenses
  , indexBasedL
  , keyBasedL
  -- ** 'DirectoryType'
  , DirectoryType (..)
  -- ** 'SocketClass'
  , SocketClass (..)
  -- ** 'Logger'
  , Logger
  -- * Internal events passed to the 'logger' .
  , Event (..)
  ) where
import Database.Postgres.Temp.Internal.Config
import Database.Postgres.Temp.Internal.Core
