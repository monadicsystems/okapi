{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}

module Okapi.Server.Failure where

import qualified Control.Monad.Except as Except
import qualified Okapi.Server.Response as Response

data Failure = Next | Abort Response.Response

instance Show Failure where
  show :: Failure -> String
  show Next = "Using next server"
  show (Abort _) = "Error returned"

next :: Except.MonadError Failure m => m a
next = Except.throwError Next

throw :: Except.MonadError Failure m => Response.Response -> m a
throw = Except.throwError . Abort

(<!>) :: Except.MonadError Failure m => m a -> m a -> m a
parser1 <!> parser2 = Except.catchError parser1 (const parser2)

guardThrow :: Except.MonadError Failure m => Response.Response -> Bool -> m ()
guardThrow _ True = pure ()
guardThrow response False = throw response

is :: (Eq a, Except.MonadError Failure m) => m a -> a -> m ()
is action desired = satisfies action (desired ==)

satisfies :: Except.MonadError Failure m => m a -> (a -> Bool) -> m ()
satisfies action predicate = do
  value <- action
  if predicate value
    then pure ()
    else next
