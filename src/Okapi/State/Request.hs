module Okapi.State.Request where

import qualified Okapi.Type.Request as Request

class Monad m => StateM m where
  get :: m Request.Request
  put :: Request.Request -> m ()

gets :: StateM m => (Request.Request -> a) -> m a
gets projection = projection <$> get

modify :: StateM m => (Request.Request -> Request.Request) -> m ()
modify modifier = do
  request <- get
  put $ modifier request

look :: StateM m => m a -> m a
look action = do
  request <- get
  result <- action
  put request
  pure result
