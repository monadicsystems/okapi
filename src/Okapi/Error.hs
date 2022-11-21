{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}

module Okapi.Error
  ( Error (..),
    Status,
    Headers,
    Header,
    Body (..),
    next,
    throw,
    (<!>),
    guardThrow,
    is,
    satisfies,
  )
where

import qualified Control.Monad.Except as Except
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified GHC.Natural as Natural
import qualified Network.HTTP.Types as HTTP
import qualified Okapi.Event as Event

data Error
  = Next
  | Error
      { status :: Status,
        headers :: Headers,
        body :: Body
      }

instance Show Error where
  show :: Error -> String
  show Next = "Using next server"
  show Error {} = "Error returned"

type Status = Natural.Natural

type Header = (HTTP.HeaderName, BS.ByteString)

type Headers = [Header]

-- | Represents the body of an HTTP Error response.
data Body
  = Raw LBS.ByteString
  | File FilePath
  | EventSource Event.EventSource

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
  if predicate value
    then pure ()
    else next
