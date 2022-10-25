{-# LANGUAGE FlexibleContexts #-}

module Okapi.Effect.Failure where

import qualified Control.Monad.Except as Except
import qualified Okapi.Type.Failure as Failure
import qualified Okapi.Type.Response as Response

next :: Except.MonadError Failure.Failure m => m a
next = Except.throwError Failure.Next

throw :: Except.MonadError Failure.Failure m => Response.Response -> m a
throw = Except.throwError . Failure.Abort

(<!>) :: Except.MonadError Failure.Failure m => m a -> m a -> m a
parser1 <!> parser2 = Except.catchError parser1 (const parser2)

guardThrow :: Except.MonadError Failure.Failure m => Response.Response -> Bool -> m ()
guardThrow _ True = pure ()
guardThrow response False = throw response

is :: (Eq a, Except.MonadError Failure.Failure m) => m a -> a -> m ()
is action desired = satisfies action (desired ==)

satisfies :: Except.MonadError Failure.Failure m => m a -> (a -> Bool) -> m ()
satisfies action predicate = do
  value <- action
  if predicate value
    then pure ()
    else next
