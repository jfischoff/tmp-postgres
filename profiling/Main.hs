import Database.Postgres.Temp
import Control.Exception

_withLoop :: IO ()
_withLoop = either throwIO pure =<< with (const $ pure ())

withCacheLoop :: IO ()
withCacheLoop = withDbCache $ \cache -> either throwIO pure =<<
  withConfig (defaultConfig <> cacheConfig cache) (const $ pure ())

main :: IO ()
main = withCacheLoop
