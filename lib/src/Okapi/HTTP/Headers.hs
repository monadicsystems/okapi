{-# LANGUAGE UndecidableInstances #-}

module Okapi.HTTP.Headers (
    MediaType (..),
    mediaTypeBytes,
    ConstF (..),
    fieldToHeaderName,
) where

import Data.ByteString (ByteString)
import Data.CaseInsensitive qualified as CI
import Data.Text qualified as Text
import Data.Text.Encoding (encodeUtf8)
import GHC.TypeLits (Symbol)
import Network.HTTP.Types qualified as HTTP

data MediaType
    = JSON
    | HTML
    | PlainText
    | FormUrlEncoded
    | OctetStream
    | EventStream
    | Custom ByteString
    deriving (Eq, Show)

mediaTypeBytes :: MediaType -> ByteString
mediaTypeBytes JSON           = "application/json"
mediaTypeBytes HTML           = "text/html"
mediaTypeBytes PlainText      = "text/plain"
mediaTypeBytes FormUrlEncoded = "application/x-www-form-urlencoded"
mediaTypeBytes OctetStream    = "application/octet-stream"
mediaTypeBytes EventStream    = "text/event-stream"
mediaTypeBytes (Custom bs)    = bs

data ConstF (val :: Symbol) = ConstF deriving (Eq, Show)

fieldToHeaderName :: String -> HTTP.HeaderName
fieldToHeaderName = CI.mk . encodeUtf8 . Text.pack . map (\c -> if c == '_' then '-' else c)
