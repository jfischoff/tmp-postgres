{-|
This module provides functions greating a temporary postgres instance on a random port for testing.

 @
 result <- 'start' []
 case result of
   Left err -> print err
   Right tempDB -> do
      -- Do stuff
      'stop' tempDB
 @

The are few different methods for starting @postgres@ which provide different
methods of dealing with @stdout@ and @stderr@.

The start methods use a config based on the one used by [pg_tmp](http://ephemeralpg.org/), but can be overriden
by in different values to the first argument of the start functions.

WARNING!!
Ubuntu's PostgreSQL installation does not put @initdb@ on the @PATH@. We need to add it manually.
The necessary binaries are in the @\/usr\/lib\/postgresql\/VERSION\/bin\/@ directory, and should be added to the @PATH@

 > echo "export PATH=$PATH:/usr/lib/postgresql/VERSION/bin/" >> /home/ubuntu/.bashrc



-}

module Database.Postgres.Temp
  ( -- * Types

    StartError (..)
  -- * Starting @postgres@
  -- $options
  -- * Exception safe interface
  , with
  , withPlan
  -- ** Separate start and stop interface.
  , start
  , startWith
  , stop
  , defaultConfig
  -- * Starting and Stopping postgres without removing the temporary directory
  , restart
  , stopPostgres
  , withRestart
  -- * Reloading the config
  , reloadConfig
  -- * DB manipulation
  , toConnectionString
  -- * Configuration
  , Lastoid (..)
  -- **
  , DirectoryType (..)
  , PartialDirectoryType (..)
  -- ** Listening socket configuration
  , SocketClass (..)
  , PartialSocketClass (..)
  -- ** Process configuration
  , PartialProcessConfig (..)
  , ProcessConfig (..)
  -- ** @postgres@ process configuration
  , PartialPostgresPlan (..)
  , PostgresPlan (..)
  -- *** @postgres@ process handle. Includes the client options for connecting
  , PostgresProcess (..)
  -- ** Database plans. This is used to call @initdb@, @postgres@ and @createdb@
  , PartialPlan (..)
  , Plan (..)
  -- ** Top level configuration
  , Config (..)
  -- *** Main resource handle. Includes the actual plan used to start all services and the resources necessary for cleanup
  , DB (..)
  ) where
import Database.Postgres.Temp.Internal
import Database.Postgres.Temp.Internal.Core
import Database.Postgres.Temp.Internal.Partial


{- $options
'startWithHandles' is the most general way to start postgres. It allows the user to
pass in it's own handles for @stdout@ and @stderr@. 'start' and 'startAndLogToTmp'
both call 'startWithHandles' passing in different handles. 'start' uses the current
process's @stdout@ and @stderr@ and 'startAndLogToTmp' logs to files in the 'mainDir'.

@postgres@ is started with a default config with the following options:

 @
   listen_addresses = ''
   shared_buffers = 12MB
   fsync = off
   synchronous_commit = off
   full_page_writes = off
   log_min_duration_statement = 0
   log_connections = on
   log_disconnections = on
   unix_socket_directories = {mainDir}
   client_min_messages = ERROR
 @

 Any of the options can be overriden by passing in a different value when starting

 @
   Right db <- 'start' [("log_min_duration_statement", "1000")]
 @

-}