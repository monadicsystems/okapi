module Okapi.State.Request.Method where

import qualified Okapi.Type.Request as Request

class Monad m => MonadState m where
  get :: m Request.Method
  put :: Request.Method -> m ()

gets :: MonadState m => (Request.Method -> a) -> m a
gets projection = projection <$> get

modify :: MonadState m => (Request.Method -> Request.Method) -> m ()
modify modifier = do
  request <- get
  put $ modifier request
