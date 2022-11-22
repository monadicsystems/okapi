{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

module Okapi.Internal.Request.Method
  ( State (..),
    gets,
    modify,
    look,
    Method,
    pattern GET,
    pattern POST,
    pattern PUT,
    pattern PATCH,
    pattern DELETE,
    pattern TRACE,
    pattern CONNECT,
    pattern OPTIONS,
    pattern HEAD,
  )
where

import qualified Control.Monad as Monad
import qualified Control.Monad.Combinators as Combinators
import qualified Control.Monad.Except as Except
import qualified Control.Monad.Logger as Logger
import qualified Data.ByteString as BS
import qualified Okapi.Internal.Error as Error

class Monad m => State m where
  get :: m Method
  put :: Method -> m ()

gets :: State m => (Method -> a) -> m a
gets projection = projection <$> get

modify :: State m => (Method -> Method) -> m ()
modify modifier = do
  request <- get
  put $ modifier request

look :: State m => m a -> m a
look action = do
  request <- get
  result <- action
  put request
  pure result

type Method = Maybe BS.ByteString

pattern GET :: Method
pattern GET = Just "GET"

pattern POST :: Method
pattern POST = Just "POST"

pattern PUT :: Method
pattern PUT = Just "PUT"

pattern PATCH :: Method
pattern PATCH = Just "PATCH"

pattern DELETE :: Method
pattern DELETE = Just "DELETE"

pattern TRACE :: Method
pattern TRACE = Just "TRACE"

pattern CONNECT :: Method
pattern CONNECT = Just "CONNECT"

pattern OPTIONS :: Method
pattern OPTIONS = Just "OPTIONS"

pattern HEAD :: Method
pattern HEAD = Just "HEAD"
