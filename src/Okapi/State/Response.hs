module Okapi.State.Response where

import qualified Okapi.Type.Response as Response

class Monad m => StateM m where
  get :: m Response.Response
  put :: Response.Response -> m ()

gets :: StateM m => (Response.Response -> a) -> m a
gets projection = projection <$> get

modify :: StateM m => (Response.Response -> Response.Response) -> m ()
modify modifier = do
  response <- get
  put $ modifier response

look :: StateM m => m a -> m a
look action = do
  response <- get
  result <- action
  put response
  pure result
