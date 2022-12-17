{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE PatternSynonyms #-}

module Okapi.Request.Method
  ( Parser (..),
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
    parse,
    match,
    Okapi.Request.Method.get,
    post,
    Okapi.Request.Method.head,
    Okapi.Request.Method.put,
    delete,
    trace,
    connect,
    options,
    patch,
    end,
  )
where

import qualified Control.Monad as Monad
import qualified Control.Monad.Combinators as Combinators
import qualified Control.Monad.Except as Except
import qualified Control.Monad.Logger as Logger
import qualified Okapi.Error as Error
import qualified Okapi.Internal.Error as Error
import Okapi.Internal.Request.Method


type Parser m = (Monad.MonadPlus m, Except.MonadError Error.Error m, Logger.MonadLogger m, State m)

-- $ methodParsers
--
-- These are parsers for parsing the HTTP request parse.

-- | TODO: Should it return (Maybe?) Or store methodParsed :: Bool in
-- extra field like before and fail when "Nothing" is there?
parse :: Parser m => m Method
parse = Okapi.Internal.Request.Method.get

match :: Parser m => Method -> m ()
match method = do
  method' <- parse
  Monad.guard (method == method')

-- | TODO: Add match function and parse that for all HTTP method parsers
get :: Parser m => m ()
get = match GET -- put patterns in Types

post :: Parser m => m ()
post = match POST

head :: Parser m => m ()
head = match HEAD

put :: Parser m => m ()
put = match PUT

delete :: Parser m => m ()
delete = match DELETE

trace :: Parser m => m ()
trace = match TRACE

connect :: Parser m => m ()
connect = match CONNECT

options :: Parser m => m ()
options = match OPTIONS

patch :: Parser m => m ()
patch = match PATCH

end :: Parser m => m ()
end = match Nothing
