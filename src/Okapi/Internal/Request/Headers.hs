{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Okapi.Internal.Request.Headers
  ( State (..),
    gets,
    modify,
    look,
    Headers,
    Header,
    Cookie,
    Crumb,
  )
where

import qualified Control.Monad as Monad
import qualified Control.Monad.Except as Except
import qualified Control.Monad.Logger as Logger
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Char8 as Char8
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Foldable as Foldable
import qualified Data.List as List
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Encoding.Base64 as Text
import qualified Network.HTTP.Types.Header as HTTP
import qualified Okapi.Internal.Error as Error
import qualified Web.Cookie as Web

class Monad m => State m where
  get :: m Headers
  put :: Headers -> m ()

gets :: State m => (Headers -> a) -> m a
gets projection = projection <$> get

modify :: State m => (Headers -> Headers) -> m ()
modify modifier = do
  request <- get
  put $ modifier request

look :: State m => m a -> m a
look action = do
  request <- get
  result <- action
  put request
  pure result

type Headers = [Header]

type Header = (HTTP.HeaderName, BS.ByteString)

type Cookie = [Crumb]

type Crumb = (BS.ByteString, BS.ByteString)
