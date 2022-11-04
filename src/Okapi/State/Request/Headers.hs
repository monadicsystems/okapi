module Okapi.State.Request.Headers where

import qualified Okapi.Type.Request as Request

class Monad m => MonadState m where
  get :: m Request.Headers
  put :: Request.Headers -> m ()

gets :: MonadState m => (Request.Headers -> a) -> m a
gets projection = projection <$> get

modify :: MonadState m => (Request.Headers -> Request.Headers) -> m ()
modify modifier = do
  request <- get
  put $ modifier request
