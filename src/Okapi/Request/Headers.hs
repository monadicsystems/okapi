{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Okapi.Request.Headers
  ( MonadHeaders (..),
    State (..),
    gets,
    modify,
    look,
    Headers,
    Header,
    Cookie,
    Crumb,
    headers,
    header,
    headersEnd,
    cookie,
    crumb,
    cookieEnd,
    basicAuth,
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
import qualified Okapi.Error as Error
import qualified Web.Cookie as Web

type MonadHeaders m = (Monad.MonadPlus m, Except.MonadError Error.Error m, Logger.MonadLogger m, State m)

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

-- $headerParsers
--
-- These are header parsers.

headers :: MonadHeaders m => m Headers
headers = do
  headers <- get
  put []
  pure headers

header :: MonadHeaders m => HTTP.HeaderName -> m Char8.ByteString
header headerName = do
  maybeHeader <- gets (Foldable.find (\(headerName', _) -> headerName == headerName'))
  case maybeHeader of
    Nothing -> Error.next
    Just header@(_, headerValue) -> do
      modify $ List.delete header
      pure headerValue

headersEnd :: MonadHeaders m => m ()
headersEnd = do
  currentHeaders <- headers
  if List.null currentHeaders
    then pure ()
    else Error.next

cookie :: MonadHeaders m => m Cookie
cookie = do
  cookieValue <- header "Cookie"
  pure $ Web.parseCookies cookieValue

crumb :: MonadHeaders m => BS.ByteString -> m BS.ByteString
crumb name = do
  cookieValue <- cookie
  case List.lookup name cookieValue of
    Nothing -> Error.next
    Just crumbValue -> do
      let crumb = (name, crumbValue)
      -- TODO: Needs testing to see if state is restored properly
      modify (\headers -> ("Cookie", LBS.toStrict $ Builder.toLazyByteString $ Web.renderCookies $ List.delete crumb cookieValue) : headers)
      pure crumbValue

cookieEnd :: MonadHeaders m => m ()
cookieEnd = do
  currentCookie <- cookie
  if List.null currentCookie
    then pure ()
    else Error.next

basicAuth :: MonadHeaders m => m (Text.Text, Text.Text)
basicAuth = do
  authValue <- header "Authorization"
  case Text.words $ Text.decodeUtf8 authValue of
    ["Basic", encodedCreds] ->
      case Text.decodeBase64 encodedCreds of
        Left _ -> Error.next
        Right decodedCreds ->
          case Text.split (== ':') decodedCreds of
            [userID, password] -> pure (userID, password)
            _ -> Error.next
    _ -> Error.next
