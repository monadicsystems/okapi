{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Okapi.Internal.Response
  ( State (..),
    gets,
    modify,
    look,
    Response (..),
    Status,
    Headers,
    Header,
    Body (..),
    Writeable (..),
  )
where

import qualified Control.Monad as Monad
import qualified Control.Monad.Except as Except
import qualified Control.Monad.Logger as Logger
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified GHC.Natural as Natural
import qualified Network.HTTP.Types as HTTP
import qualified Okapi.Internal.Error as Error
import qualified Okapi.Internal.Event as Event
import qualified Web.Cookie as Web

class Monad m => State m where
  get :: m Response
  put :: Response -> m ()

gets :: State m => (Response -> a) -> m a
gets projection = projection <$> get

modify :: State m => (Response -> Response) -> m ()
modify modifier = do
  response <- get
  put $ modifier response

look :: State m => m a -> m a
look action = do
  response <- get
  result <- action
  put response
  pure result

-- | Represents HTTP responses that can be returned by a parser.
data Response = Response
  { status :: Status,
    headers :: Headers,
    body :: Body
  }

type Status = Natural.Natural

type Headers = [Header]

type Header = (HTTP.HeaderName, BS.ByteString)

-- | Represents the body of an HTTP response.
data Body
  = Raw LBS.ByteString
  | File FilePath
  | EventSource Event.EventSource

class Writeable a where
  toLBS :: a -> LBS.ByteString
  default toLBS :: Show a => a -> LBS.ByteString
  toLBS = LBS.fromStrict . Text.encodeUtf8 . Text.pack . Prelude.takeWhile ('"' /=) . Prelude.dropWhile ('"' ==) . show

instance Writeable Text.Text

instance Writeable LBS.ByteString where
  toLBS :: LBS.ByteString -> LBS.ByteString
  toLBS = id

instance Writeable Int
