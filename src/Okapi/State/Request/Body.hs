module Okapi.State.Request.Body where

import qualified Okapi.Type.Request as Request

class Monad m => MonadState m where
  get :: m Request.Body
  put :: Request.Body -> m ()

gets :: MonadState m => (Request.Body -> a) -> m a
gets projection = projection <$> get

modify :: MonadState m => (Request.Body -> Request.Body) -> m ()
modify modifier = do
  request <- get
  put $ modifier request
