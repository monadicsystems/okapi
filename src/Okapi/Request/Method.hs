{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

module Okapi.Request.Method
  ( MonadMethod (..),
    State (..),
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
    method,
    methodGET,
    methodPOST,
    methodHEAD,
    methodPUT,
    methodDELETE,
    methodTRACE,
    methodCONNECT,
    methodOPTIONS,
    methodPATCH,
    methodEnd,
  )
where

import qualified Control.Monad as Monad
import qualified Control.Monad.Combinators as Combinators
import qualified Control.Monad.Except as Except
import qualified Control.Monad.Logger as Logger
import qualified Data.ByteString as BS
import qualified Okapi.Error as Error
import qualified Okapi.Log as Log

type MonadMethod m = (Monad.MonadPlus m, Except.MonadError Error.Error m, Logger.MonadLogger m, State m)

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

-- $ methodParsers
--
-- These are parsers for parsing the HTTP request method.

-- | TODO: Should it return (Maybe?) Or store methodParsed :: Bool in
-- extra field like before and fail when "Nothing" is there?
method :: MonadMethod m => m Method
method = do
  Log.logIt "Parsing method"
  method <- get
  Log.logIt $ "Parsed method: " <> show method
  pure method

methodGET :: MonadMethod m => m ()
methodGET = Error.is method GET -- put patterns in Types

methodPOST :: MonadMethod m => m ()
methodPOST = Error.is method POST

methodHEAD :: MonadMethod m => m ()
methodHEAD = Error.is method HEAD

methodPUT :: MonadMethod m => m ()
methodPUT = Error.is method PUT

methodDELETE :: MonadMethod m => m ()
methodDELETE = Error.is method DELETE

methodTRACE :: MonadMethod m => m ()
methodTRACE = Error.is method TRACE

methodCONNECT :: MonadMethod m => m ()
methodCONNECT = Error.is method CONNECT

methodOPTIONS :: MonadMethod m => m ()
methodOPTIONS = Error.is method OPTIONS

methodPATCH :: MonadMethod m => m ()
methodPATCH = Error.is method PATCH

methodEnd :: MonadMethod m => m ()
methodEnd = do
  Log.logIt "Checking if method has been parsed"
  maybeMethod <- Combinators.optional method
  case maybeMethod of
    Nothing -> Log.logIt "The method is Nothing"
    Just _ -> do
      Log.logIt "There is still a method"
      Error.next
