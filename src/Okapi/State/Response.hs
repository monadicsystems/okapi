{-# LANGUAGE InstanceSigs #-}

module Okapi.State.Response where

import qualified Control.Monad.Except as Except
import qualified Control.Monad.State.Strict as State
import qualified Okapi.Type.HTTP as HTTP
import qualified Okapi.Type.Response as Response

class Monad m => MonadState m where
  get :: m Response.Response
  put :: Response.Response -> m ()

gets :: MonadState m => (Response.Response -> a) -> m a
gets projection = projection <$> get

modify :: MonadState m => (Response.Response -> Response.Response) -> m ()
modify modifier = do
  response <- get
  put $ modifier response

look :: MonadState m => m a -> m a
look action = do
  response <- get
  result <- action
  put response
  pure result

instance Monad m => MonadState (HTTP.HTTPT m) where
  get :: Monad m => HTTP.HTTPT m Response.Response
  get =
    HTTP.HTTPT
      . Except.ExceptT
      . State.StateT
      $ \state@(_, res) -> pure (Right res, state)
  put :: Monad m => Response.Response -> HTTP.HTTPT m ()
  put newRes =
    HTTP.HTTPT
      . Except.ExceptT
      . State.StateT
      $ \(req, _) -> pure (Right (), (req, newRes))
