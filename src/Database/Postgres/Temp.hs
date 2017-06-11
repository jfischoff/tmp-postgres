{-# LANGUAGE RecordWildCards, LambdaCase, ScopedTypeVariables #-}
module Database.Postgres.Temp
  ( DB (..)
  , start
  , startAndLogToTmp
  , startWithHandles
  , stop
  ) where
import Database.Postgres.Temp.Internal