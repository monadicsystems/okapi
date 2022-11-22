{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Okapi.Request.Method
  ( Parser (..),
    use,
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
import qualified Okapi.Log as Log

type Parser m = (Monad.MonadPlus m, Except.MonadError Error.Error m, Logger.MonadLogger m, State m)

-- $ methodParsers
--
-- These are parsers for parsing the HTTP request use.

-- | TODO: Should it return (Maybe?) Or store methodParsed :: Bool in
-- extra field like before and fail when "Nothing" is there?
use :: Parser m => m Method
use = do
  Log.logIt "Parsing use"
  use <- Okapi.Internal.Request.Method.get
  Log.logIt $ "Parsed use: " <> show use
  pure use

-- | TODO: Add match function and use that for all HTTP method parsers
get :: Parser m => m ()
get = Error.is use GET -- put patterns in Types

post :: Parser m => m ()
post = Error.is use POST

head :: Parser m => m ()
head = Error.is use HEAD

put :: Parser m => m ()
put = Error.is use PUT

delete :: Parser m => m ()
delete = Error.is use DELETE

trace :: Parser m => m ()
trace = Error.is use TRACE

connect :: Parser m => m ()
connect = Error.is use CONNECT

options :: Parser m => m ()
options = Error.is use OPTIONS

patch :: Parser m => m ()
patch = Error.is use PATCH

end :: Parser m => m ()
end = do
  Log.logIt "Checking if use has been parsed"
  maybeMethod <- Combinators.optional use
  case maybeMethod of
    Nothing -> Log.logIt "The use is Nothing"
    Just _ -> do
      Log.logIt "There is still a use"
      Error.next
