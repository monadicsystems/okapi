{-# LANGUAGE FlexibleContexts #-}

module Okapi.HTTP.Error
  ( Error (..),
    Body,
    Status,
    Headers,
    Header,
    next,
    throw,
    (<!>),
    guardThrow,
    is,
    satisfies,
  )
where

import qualified Control.Monad as Monad
import qualified Control.Monad.Except as Except
import Okapi.Internal.Error

next :: Except.MonadError Error m => m a
next = Except.throwError Next

throw :: Except.MonadError Error m => Status -> Headers -> Body -> m a
throw status headers = Except.throwError . Error status headers

(<!>) :: Except.MonadError Error m => m a -> m a -> m a
parser1 <!> parser2 = Except.catchError parser1 (const parser2)

guardThrow :: Except.MonadError Error m => Status -> Headers -> Body -> Bool -> m ()
guardThrow _ _ _ True = pure ()
guardThrow status headers body False = throw status headers body

is :: (Eq a, Except.MonadError Error m) => m a -> a -> m ()
is action desired = satisfies action (desired ==)

satisfies :: Except.MonadError Error m => m a -> (a -> Bool) -> m ()
satisfies action predicate = do
  value <- action
  Monad.unless (predicate value) next
