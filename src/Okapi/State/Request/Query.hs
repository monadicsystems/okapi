module Okapi.State.Request.Query where

import qualified Okapi.Type.Request as Request

class Monad m => MonadState m where
  get :: m Request.Query
  put :: Request.Query -> m ()

gets :: MonadState m => (Request.Query -> a) -> m a
gets projection = projection <$> get

modify :: MonadState m => (Request.Query -> Request.Query) -> m ()
modify modifier = do
  request <- get
  put $ modifier request
