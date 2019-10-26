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

rmDirIgnoreErrors :: FilePath -> IO ()
rmDirIgnoreErrors mainDir =
  removeDirectoryRecursive mainDir `catch` (\(_ :: IOException) -> return ())

waitForDB :: PostgresClient.Options -> IO ()
waitForDB options = do
  let theConnectionString = PostgresClient.toConnectionString options
  print theConnectionString
  try (bracket (PG.connectPostgreSQL theConnectionString) PG.close mempty) >>= \case
    Left (_ :: IOError) -> threadDelay 10000 >> waitForDB options
    Right () -> return ()

throwIfNotSuccess :: Exception e => (ExitCode -> e) -> ExitCode -> IO ()
throwIfNotSuccess f = \case
  ExitSuccess -> pure ()
  e -> throwIO $ f e


