import Database.Postgres.Temp
import Database.Postgres.Temp.Internal.Core
import Control.Exception
import qualified Control.Monad as M

withLoop :: IO ()
withLoop = either throwIO pure =<< with (const $ pure ())

withCacheLoop :: IO ()
withCacheLoop = withDbCache $ \cache -> either throwIO pure =<<
  withConfig (defaultConfig <> cacheConfig cache) (const $ pure ())

main :: IO ()
main = timeIt "main" withCacheLoop
