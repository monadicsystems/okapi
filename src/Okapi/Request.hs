{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}

module Okapi.Request
  ( MonadRequest (..),
    State (..),
    gets,
    modify,
    look,
    Request (..),
  )
where

import qualified Control.Monad as Monad
import qualified Control.Monad.Except as Except
import qualified Control.Monad.Logger as Logger
import qualified Data.Vault.Lazy as Vault
import qualified Okapi.Error as Error
import qualified Okapi.Request.Body as Body
import qualified Okapi.Request.Headers as Headers
import qualified Okapi.Request.Method as Method
import qualified Okapi.Request.Path as Path
import qualified Okapi.Request.Query as Query
import qualified Okapi.Request.Vault as Vault

type MonadRequest m = (Except.MonadError Error.Error m, Logger.MonadLogger m, Monad.MonadPlus m, State m)

class (Method.State m, Path.State m, Query.State m, Body.State m, Headers.State m, Vault.State m) => State m where
  get :: m Request
  default get :: m Request
  get = Request <$> Method.get <*> Path.get <*> Query.get <*> Body.get <*> Headers.get <*> Vault.get
  put :: Request -> m ()
  default put :: Request -> m ()
  put (Request method path query body headers vault) = do
    Method.put method
    Path.put path
    Query.put query
    Body.put body
    Headers.put headers
    Vault.put vault

gets :: State m => (Request -> a) -> m a
gets projection = projection <$> get

modify :: State m => (Request -> Request) -> m ()
modify modifier = do
  response <- get
  put $ modifier response

look :: State m => m a -> m a
look action = do
  response <- get
  result <- action
  put response
  pure result

data Request = Request
  { method :: Method.Method,
    path :: Path.Path,
    query :: Query.Query,
    body :: Body.Body,
    headers :: Headers.Headers,
    vault :: Vault.Vault
  }

-- | Parses the entire request.
-- request :: MonadRequest m => m Request
-- request = Request <$> Method.method <*> Path.path <*> Query.query <*> Body.body <*> Headers.headers <*> Vault.vault

-- requestEnd :: MonadRequest m => m ()
-- requestEnd = do
--   Method.methodEnd
--   Path.pathEnd
--   Query.queryEnd
--   Headers.headersEnd
--   Body.bodyEnd
