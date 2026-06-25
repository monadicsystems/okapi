{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}

module Okapi.Protocol.Body (
    Body (..),
    IsoJson,
    ParseError (..),
    parse,
    printM,
    raw,
    json,
    form,
    html,
    noContent,
) where

import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as LBS
import Data.Kind (Type)
import Data.OpenApi (ToSchema)
import Lucid (Html, renderBS)
import Okapi.Codec (Codec (..), ForRequest, ForResponse, ParseErrorOf, StateOf)
import Okapi.Codec qualified as Codec
import Web.FormUrlEncoded (FromForm, ToForm, urlDecodeAsForm, urlEncodeAsForm)

type IsoJson a = (Aeson.FromJSON a, Aeson.ToJSON a, ToSchema a)

type Body :: Type -> Type -> Type
data Body ctx a where
    Raw       :: Body ctx (IO LBS.ByteString)
    Json      :: IsoJson a => Body ctx (IO a)
    NoContent :: Body ctx (IO ())
    Form      :: (ToForm a, FromForm a) => Body ForRequest (IO a)
    Html      :: Body ForResponse (IO (Html ()))

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
    bodyAlg Html     _  = (Left ParseError, LBS.empty)
    bodyAlg NoContent _ = (Right (pure ()), LBS.empty)

printM :: forall ctx i o. Codec (Body ctx) i o -> i -> IO LBS.ByteString
printM = go
  where
    go :: forall i' o'. Codec (Body ctx) i' o' -> i' -> IO LBS.ByteString
    go (Pure _)          _     = pure mempty
    go (FMap _ c)        i     = go c i
    go (LMap f c)        i     = go c (f i)
    go (Apply cf cx)     i     = liftA2 (<>) (go cf i) (go cx i)
    go (Lift Raw)        ioLbs = ioLbs
    go (Lift Json)       ioA   = Aeson.encode <$> ioA
    go (Lift Form)       ioA   = urlEncodeAsForm <$> ioA
    go (Lift Html)       ioH   = renderBS <$> ioH
    go (Lift NoContent)  _     = pure mempty

raw :: Codec (Body ctx) (IO LBS.ByteString) (IO LBS.ByteString)
raw = Lift Raw

json :: IsoJson a => Codec (Body ctx) (IO a) (IO a)
json = Lift Json

form :: (ToForm a, FromForm a) => Codec (Body ForRequest) (IO a) (IO a)
form = Lift Form

html :: Codec (Body ForResponse) (IO (Html ())) (IO (Html ()))
html = Lift Html

noContent :: Codec (Body ctx) (IO ()) (IO ())
noContent = Lift NoContent

