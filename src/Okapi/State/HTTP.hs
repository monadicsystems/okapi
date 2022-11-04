{-# LANGUAGE ConstraintKinds #-}
module Okapi.State.HTTP where

import qualified Okapi.State.Request as Request
import qualified Okapi.State.Response as Response
import qualified Okapi.Type.Request as Request
import qualified Okapi.Type.Response as Response

type MonadState m = (Request.MonadState m, Response.MonadState m)

get :: MonadState m => m (Request.Request, Response.Response)
get = undefined

put :: MonadState m => (Request.Request, Response.Response) -> m ()
put = undefined

gets :: MonadState m => ((Request.Request, Response.Response) -> a) -> m a
gets projection = projection <$> get

modify :: MonadState m => ((Request.Request, Response.Response) -> (Request.Request, Response.Response)) -> m ()
modify modifier = do
  requestAndResponse <- get
  put $ modifier requestAndResponse
