{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}

module Okapi.Internal.Request
  ( State (..),
    Request (..),
    gets,
    modify,
    look,
  )
where

import qualified Control.Monad as Monad
import qualified Control.Monad.Except as Except
import qualified Control.Monad.Logger as Logger
import qualified Data.Vault.Lazy as Vault
import qualified Okapi.Internal.Error as Error
import qualified Okapi.Internal.Request.Body as Body
import qualified Okapi.Internal.Request.Headers as Headers
import qualified Okapi.Internal.Request.Method as Method
import qualified Okapi.Internal.Request.Path as Path
import qualified Okapi.Internal.Request.Query as Query
import qualified Okapi.Internal.Request.Vault as Vault

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

data Request = Request
  { method :: Method.Method,
    path :: Path.Path,
    query :: Query.Query,
    body :: Body.Body,
    headers :: Headers.Headers,
    vault :: Vault.Vault
  }

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
