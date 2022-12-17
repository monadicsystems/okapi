{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Okapi.Request.Headers
  ( Parser (..),
    Headers,
    Header,
    parse,
    value,
    end,
    basicAuth,
  )
where

import qualified Control.Monad as Monad
import qualified Control.Monad.Except as Except
import qualified Control.Monad.Logger as Logger
import qualified Data.ByteString.Char8 as Char8
import qualified Data.Foldable as Foldable
import qualified Data.List as List
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Encoding.Base64 as Text
import qualified Network.HTTP.Types as HTTP
import qualified Okapi.Error as Error
import qualified Okapi.Internal.Error as Error
import Okapi.Internal.Request.Headers

type Parser m = (Monad.MonadPlus m, Except.MonadError Error.Error m, Logger.MonadLogger m, State m)

-- $headerParsers
--
-- These are lookup parsers.

parse :: Parser m => m Headers
parse = do
  header <- get
  put []
  pure header

value :: Parser m => HTTP.HeaderName -> m Char8.ByteString
value headerName = do
  maybeHeader <- gets (List.find (\(headerName', _) -> headerName == headerName'))
  case maybeHeader of
    Nothing -> Error.next
    Just header@(_, headerValue) -> do
      modify $ List.delete header
      pure headerValue

end :: Parser m => m ()
end = do
  currentHeaders <- parse
  if List.null currentHeaders
    then pure ()
    else Error.next

basicAuth :: Parser m => m (Text.Text, Text.Text)
basicAuth = do
  authValue <- value "Authorization"
  case Text.words $ Text.decodeUtf8 authValue of
    ["Basic", encodedCreds] ->
      case Text.decodeBase64 encodedCreds of
        Left _ -> Error.next
        Right decodedCreds ->
          case Text.split (== ':') decodedCreds of
            [userID, password] -> pure (userID, password)
            _ -> Error.next
    _ -> Error.next
