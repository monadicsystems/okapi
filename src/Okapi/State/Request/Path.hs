module Okapi.State.Request.Path where

import qualified Okapi.Type.Request as Request

class Monad m => MonadState m where
  get :: m Request.Path
  put :: Request.Path -> m ()

gets :: MonadState m => (Request.Path -> a) -> m a
gets projection = projection <$> get

modify :: MonadState m => (Request.Path -> Request.Path) -> m ()
modify modifier = do
  request <- get
  put $ modifier request
