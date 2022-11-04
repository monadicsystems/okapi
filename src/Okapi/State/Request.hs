{-# LANGUAGE ConstraintKinds #-}

module Okapi.State.Request where

import qualified Okapi.State.Request.Body as Body
import qualified Okapi.State.Request.Headers as Headers
import qualified Okapi.State.Request.Method as Method
import qualified Okapi.State.Request.Path as Path
import qualified Okapi.State.Request.Query as Query
import qualified Okapi.State.Request.Vault as Vault
import qualified Okapi.Type.Request as Request

type MonadState m = (Method.MonadState m, Path.MonadState m, Query.MonadState m, Headers.MonadState m, Body.MonadState m, Vault.MonadState m)

get :: MonadState m => m Request.Request
get = undefined

put :: MonadState m => Request.Request -> m ()
put = undefined

gets :: MonadState m => (Request.Request -> a) -> m a
gets projection = projection <$> get

modify :: MonadState m => (Request.Request -> Request.Request) -> m ()
modify modifier = do
  request <- get
  put $ modifier request

look :: MonadState m => m a -> m a
look action = do
  request <- get
  result <- action
  put request
  pure result
