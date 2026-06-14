{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}

module Okapi.Body (
    Body (..),
    ForRequest,
    ForResponse,
    NoContent (..),
    IsoJson,
    ParseError (..),
    parse,
    printM,
    raw,
    json,
    noContent,
    HasBody (..),
) where

import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as LBS
import Data.Kind (Type)
import Data.OpenApi (ToSchema)
import Okapi.Codec (Codec (..), IsoCodec (..), ParseErrorOf, StateOf)
import Okapi.Codec qualified as Codec
import Okapi.Headers (ForRequest, ForResponse)

type IsoJson a = (Aeson.FromJSON a, Aeson.ToJSON a, ToSchema a)

data NoContent = NoContent deriving (Eq, Show)

type Body :: Type -> Type -> Type
data Body ctx a where
    Raw   :: Body ctx (IO LBS.ByteString)
    Json  :: IsoJson a => Body ctx (IO a)
    Empty :: Body ctx (IO NoContent)

data ParseError = ParseError deriving (Eq, Show)

type instance StateOf      (Body ctx) = LBS.ByteString
type instance ParseErrorOf (Body ctx) = ParseError

parse :: forall ctx i o. Codec (Body ctx) i o -> LBS.ByteString -> (Either ParseError o, LBS.ByteString)
parse = Codec.parser bodyAlg
  where
    bodyAlg :: forall a. Body ctx a -> LBS.ByteString -> (Either ParseError a, LBS.ByteString)
    bodyAlg Raw   bs = (Right (pure bs), LBS.empty)
    bodyAlg Json  bs = case Aeson.eitherDecode bs of
        Left _  -> (Left ParseError, bs)
        Right x -> (Right (pure x), LBS.empty)
    bodyAlg Empty _  = (Right (pure NoContent), LBS.empty)

printM :: forall ctx i o. Codec (Body ctx) i o -> i -> IO LBS.ByteString
printM = go
  where
    go :: forall i' o'. Codec (Body ctx) i' o' -> i' -> IO LBS.ByteString
    go (Pure _)      _ = pure mempty
    go (FMap _ c)    i = go c i
    go (LMap f c)    i = go c (f i)
    go (Apply cf cx) i = liftA2 (<>) (go cf i) (go cx i)
    go (Embed Raw)   ioLbs = ioLbs
    go (Embed Json)  ioA   = Aeson.encode <$> ioA
    go (Embed Empty) _     = pure mempty

raw :: Codec (Body ctx) (IO LBS.ByteString) (IO LBS.ByteString)
raw = Embed Raw

json :: IsoJson a => Codec (Body ctx) (IO a) (IO a)
json = Embed Json

noContent :: Codec (Body ctx) (IO NoContent) (IO NoContent)
noContent = Embed Empty

class HasBody (contract :: Type -> Type -> Type) where
    type BodyCtx contract :: Type
    body ::
        Codec (Body (BodyCtx contract)) (IO b) (IO b) ->
        contract h LBS.ByteString ->
        contract h b
