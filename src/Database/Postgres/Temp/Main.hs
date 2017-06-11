{-# LANGUAGE RecordWildCards, LambdaCase, ScopedTypeVariables #-}
module Database.Postgres.Temp.Main where
import Database.Postgres.Temp.Internal
import System.Environment
import System.IO
import System.Exit

data Command = Start | Stop String

parseArgs :: [String] -> Maybe Command
parseArgs = \case
  []  -> return Start
  [x] -> return $ Stop x
  _ -> Nothing

printHelp :: IO a
printHelp = do
  putStrLn $  "Too many arguments!"
    ++ "Pass in a zero arguments to start or a connection string to stop"
  exitWith $ ExitFailure 64

run :: Command -> IO ()
run = \case
  Start -> start >>= \case
    Left err -> case err of
      InitDBFailed   x -> do
        hPutStrLn stderr $ "initDB failed with exit code " ++ show x
        exitWith $ ExitFailure 69 -- EX_UNAVAILABLE
      CreateDBFailed x -> do
        hPutStrLn stderr $ "createDB failed with exit code " ++ show x
        exitWith $ ExitFailure 69 -- EX_UNAVAILABLE
    Right x -> putStrLn $ connectionString x
  Stop connStr -> stopWithConnectionString connStr >>= \case
    Success -> return ()
    TimedOut time -> hPutStrLn stderr $
      "postgres did not respond to SIGTERM in a timely manner. " ++
      "Killed after " ++ show time ++ " microseconds."
    ErrorCode x -> hPutStrLn stderr $
      "postgres returned an error code when shutting down: " ++ show x
    Failed msg -> do
      hPutStrLn stderr $ "Failed to stop postgres: " ++ msg
      exitWith $ ExitFailure 69

main :: IO ()
main = do
  xs <- getArgs
  cmd <- maybe printHelp return $ parseArgs xs
  run cmd