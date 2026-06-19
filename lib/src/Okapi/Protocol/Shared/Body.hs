{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}

module Okapi.Protocol.Shared.Body (
    Body (..),
    ForRequest,
    ForResponse,
    IsoJson,
    ParseError (..),
    parse,
    printM,
    raw,
    json,
    form,
    html,
    noContent,
    mediaType,
    bodyMediaType,
    HasBody (..),
) where

import Data.Aeson qualified as Aeson
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as LBS
import Data.Kind (Type)
import Data.OpenApi (ToSchema)
import Lucid (Html, renderBS)
import Okapi.Codec (Codec (..), ParseErrorOf, StateOf)
import Okapi.Codec qualified as Codec
import Okapi.Protocol.Shared.Headers (ForRequest, ForResponse)
import Web.FormUrlEncoded (FromForm, ToForm, urlDecodeAsForm, urlEncodeAsForm)

type IsoJson a = (Aeson.FromJSON a, Aeson.ToJSON a, ToSchema a)

type Body :: Type -> Type -> Type
data Body ctx a where
    Raw      :: Body ctx (IO LBS.ByteString)
    Json     :: IsoJson a => Body ctx (IO a)
    -- | @application/x-www-form-urlencoded@ request body. Request-only.
    Form     :: (ToForm a, FromForm a) => Body ForRequest (IO a)
    -- | @text/html@ response body (rendered via lucid). Response-only, print-only.
    Html     :: Body ForResponse (IO (Html ()))
    NoContent :: Body ctx (IO ())

data ParseError = ParseError deriving (Eq, Show)

type instance StateOf      (Body ctx) = LBS.ByteString
type instance ParseErrorOf (Body ctx) = ParseError

parse :: forall ctx i o. Codec (Body ctx) i o -> LBS.ByteString -> (Either ParseError o, LBS.ByteString)
parse = Codec.parser bodyAlg
  where
    bodyAlg :: forall a. Body ctx a -> LBS.ByteString -> (Either ParseError a, LBS.ByteString)
    bodyAlg Raw      bs = (Right (pure bs), LBS.empty)
    bodyAlg Json     bs = case Aeson.eitherDecode bs of
        Left _  -> (Left ParseError, bs)
        Right x -> (Right (pure x), LBS.empty)
    bodyAlg Form     bs = case urlDecodeAsForm bs of
        Left _  -> (Left ParseError, bs)
        Right x -> (Right (pure x), LBS.empty)
    bodyAlg Html     bs = (Left ParseError, bs)  -- HTML is print-only; never parsed back
    bodyAlg NoContent _  = (Right (pure ()), LBS.empty)

printM :: forall ctx i o. Codec (Body ctx) i o -> i -> IO LBS.ByteString
printM = go
  where
    go :: forall i' o'. Codec (Body ctx) i' o' -> i' -> IO LBS.ByteString
    go (Pure _)         _ = pure mempty
    go (FMap _ c)       i = go c i
    go (LMap f c)       i = go c (f i)
    go (Apply cf cx)    i = liftA2 (<>) (go cf i) (go cx i)
    go (Lift Raw)      ioLbs = ioLbs
    go (Lift Json)     ioA   = Aeson.encode <$> ioA
    go (Lift Form)     ioA   = urlEncodeAsForm <$> ioA
    go (Lift Html)     ioH   = renderBS <$> ioH
    go (Lift NoContent) _    = pure mempty

-- | Raw bytes body; no encoding or decoding applied.
raw :: Codec (Body ctx) (IO LBS.ByteString) (IO LBS.ByteString)
raw = Lift Raw

-- | JSON-encoded body; requires 'Aeson.FromJSON', 'Aeson.ToJSON', and 'ToSchema' instances.
json :: IsoJson a => Codec (Body ctx) (IO a) (IO a)
json = Lift Json

-- | @application/x-www-form-urlencoded@ request body; requires 'ToForm'/'FromForm'.
--   Request-only — only typechecks on a request's body slot.
form :: (ToForm a, FromForm a) => Codec (Body ForRequest) (IO a) (IO a)
form = Lift Form

-- | @text/html@ response body (lucid). Response-only — only typechecks on a response's body slot.
--   Print-only: parsing an HTML body always fails (you never decode HTML back into 'Html').
html :: Codec (Body ForResponse) (IO (Html ())) (IO (Html ()))
html = Lift Html

-- | Empty body (no content); produces and expects zero bytes.
noContent :: Codec (Body ctx) (IO ()) (IO ())
noContent = Lift NoContent

-- | The bare media-type token a body content type carries (for the @Content-Type@ header).
--   'Raw' and 'NoContent' carry none.
mediaType :: Body ctx a -> Maybe ByteString
mediaType Json      = Just "application/json"
mediaType Form      = Just "application/x-www-form-urlencoded"
mediaType Html      = Just "text/html"
mediaType Raw       = Nothing
mediaType NoContent = Nothing

-- | The media type of a body codec (walks to its 'Lift'), if any.
bodyMediaType :: Codec (Body ctx) i o -> Maybe ByteString
bodyMediaType = go
  where
    go :: Codec (Body ctx) i' o' -> Maybe ByteString
    go (Lift b)      = mediaType b
    go (FMap _ c)    = go c
    go (LMap _ c)    = go c
    go (Apply cf cx) = case go cf of Just m -> Just m; Nothing -> go cx
    go (Pure _)      = Nothing

class HasBody (contract :: Type -> Type -> Type) where
    type BodyCtx contract :: Type
    body ::
        Codec (Body (BodyCtx contract)) (IO b) (IO b) ->
        contract h LBS.ByteString ->
        contract h b
