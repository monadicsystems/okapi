{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}

module Okapi.Internal.Error
  ( Error (..),
    Status,
    Headers,
    Header,
    Body (..),
  )
where

import qualified Control.Monad.Except as Except
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified GHC.Natural as Natural
import qualified Network.HTTP.Types as HTTP
import qualified Okapi.Internal.Event as Event

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
