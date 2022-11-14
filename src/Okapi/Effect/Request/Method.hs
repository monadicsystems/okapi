{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}

module Okapi.Effect.Request.Method where

import qualified Control.Monad as Monad
import qualified Control.Monad.Combinators as Combinators
import qualified Control.Monad.Except as Except
import qualified Control.Monad.Logger as Logger
import qualified Okapi.Effect.Failure as Failure
import qualified Okapi.Effect.Log as Log
import qualified Okapi.State.Request.Method as Method
import qualified Okapi.Type.Failure as Failure
import qualified Okapi.Type.Request as Request

type  MonadMethod m = (Monad.MonadPlus m, Except.MonadError Failure.Failure m, Logger.MonadLogger m, Method.MonadState m)

-- $ methodParsers
--
-- These are parsers for parsing the HTTP request method.

-- | TODO: Should it return (Maybe?) Or store methodParsed :: Bool in
-- extra field like before and fail when "Nothing" is there?
method :: MonadMethod m => m Request.Method
method = do
  Log.logIt "Parsing method"
  method <- Method.get
  Log.logIt $ "Parsed method: " <> show method
  pure method

methodGET :: MonadMethod m => m ()
methodGET = Failure.is method Request.GET -- put patterns in Types

methodPOST :: MonadMethod m => m ()
methodPOST = Failure.is method Request.POST

methodHEAD :: MonadMethod m => m ()
methodHEAD = Failure.is method Request.HEAD

methodPUT :: MonadMethod m => m ()
methodPUT = Failure.is method Request.PUT

methodDELETE :: MonadMethod m => m ()
methodDELETE = Failure.is method Request.DELETE

methodTRACE :: MonadMethod m => m ()
methodTRACE = Failure.is method Request.TRACE

methodCONNECT :: MonadMethod m => m ()
methodCONNECT = Failure.is method Request.CONNECT

methodOPTIONS :: MonadMethod m => m ()
methodOPTIONS = Failure.is method Request.OPTIONS

methodPATCH :: MonadMethod m => m ()
methodPATCH = Failure.is method Request.PATCH

methodEnd :: MonadMethod m => m ()
methodEnd = do
  Log.logIt "Checking if method has been parsed"
  maybeMethod <- Combinators.optional method
  case maybeMethod of
    Nothing -> Log.logIt "The method is Nothing"
    Just _ -> do
      Log.logIt "There is still a method"
      Failure.next

look :: MonadMethod m => m a -> m a
look action = do
  request <- Method.get
  result <- action
  Method.put request
  pure result
