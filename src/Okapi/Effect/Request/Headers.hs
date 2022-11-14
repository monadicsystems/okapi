{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Okapi.Effect.Request.Headers where

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
import qualified Okapi.Effect.Failure as Failure
import qualified Okapi.State.Request.Headers as Headers
import qualified Okapi.Type.Failure as Failure
import qualified Okapi.Type.Request as Request
import qualified Web.Cookie as Web

type MonadHeaders m = (Monad.MonadPlus m, Except.MonadError Failure.Failure m, Logger.MonadLogger m, Headers.MonadState m)

-- $headerParsers
--
-- These are header parsers.

headers :: MonadHeaders m => m Request.Headers
headers = do
  headers <- Headers.get
  Headers.put []
  pure headers

header :: MonadHeaders m => HTTP.HeaderName -> m Char8.ByteString
header headerName = do
  maybeHeader <- Headers.gets (Foldable.find (\(headerName', _) -> headerName == headerName'))
  case maybeHeader of
    Nothing -> Failure.next
    Just header@(_, headerValue) -> do
      Headers.modify $ List.delete header
      pure headerValue

headersEnd :: MonadHeaders m => m ()
headersEnd = do
  currentHeaders <- headers
  if List.null currentHeaders
    then pure ()
    else Failure.next

cookie :: MonadHeaders m => m Request.Cookie
cookie = do
  cookieValue <- header "Cookie"
  pure $ Web.parseCookies cookieValue

cookieCrumb :: MonadHeaders m => BS.ByteString -> m BS.ByteString
cookieCrumb name = do
  cookieValue <- cookie
  case List.lookup name cookieValue of
    Nothing -> Failure.next
    Just crumbValue -> do
      let crumb = (name, crumbValue)
      -- TODO: Needs testing to see if state is restored properly
      Headers.modify (\headers -> ("Cookie", LBS.toStrict $ Builder.toLazyByteString $ Web.renderCookies $ List.delete crumb cookieValue) : headers)
      pure crumbValue

cookieEnd :: MonadHeaders m => m ()
cookieEnd = do
  currentCookie <- cookie
  if List.null currentCookie
    then pure ()
    else Failure.next

basicAuth :: MonadHeaders m => m (Text.Text, Text.Text)
basicAuth = do
  authValue <- header "Authorization"
  case Text.words $ Text.decodeUtf8 authValue of
    ["Basic", encodedCreds] ->
      case Text.decodeBase64 encodedCreds of
        Left _ -> Failure.next
        Right decodedCreds ->
          case Text.split (== ':') decodedCreds of
            [userID, password] -> pure (userID, password)
            _ -> Failure.next
    _ -> Failure.next

look :: MonadHeaders m => m a -> m a
look action = do
  request <- Headers.get
  result <- action
  Headers.put request
  pure result
