{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module App where

import Control.Monad.Trans.Reader (ReaderT)
import qualified Database.Redis as Redis
import qualified Database.SQLite.Simple as SQLite

data Config = Config
  { redisConn :: Redis.Connection,
    sqliteConn :: SQLite.Connection
  }

type Context = ReaderT Config IO
