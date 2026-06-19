{-# LANGUAGE GADTs #-}
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
    noContent,
    HasBody (..),
) where

import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as LBS
import Data.Kind (Type)
import Data.OpenApi (ToSchema)
import Okapi.Codec (Codec (..), ParseErrorOf, StateOf)
import Okapi.Codec qualified as Codec
import Okapi.Protocol.Shared.Headers (ForRequest, ForResponse)

type IsoJson a = (Aeson.FromJSON a, Aeson.ToJSON a, ToSchema a)

type Body :: Type -> Type -> Type
data Body ctx a where
    Raw      :: Body ctx (IO LBS.ByteString)
    Json     :: IsoJson a => Body ctx (IO a)
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
    go (Lift NoContent) _    = pure mempty

-- | Raw bytes body; no encoding or decoding applied.
raw :: Codec (Body ctx) (IO LBS.ByteString) (IO LBS.ByteString)
raw = Lift Raw

-- | JSON-encoded body; requires 'Aeson.FromJSON', 'Aeson.ToJSON', and 'ToSchema' instances.
json :: IsoJson a => Codec (Body ctx) (IO a) (IO a)
json = Lift Json

-- | Empty body (no content); produces and expects zero bytes.
noContent :: Codec (Body ctx) (IO ()) (IO ())
noContent = Lift NoContent

class HasBody (contract :: Type -> Type -> Type) where
    type BodyCtx contract :: Type
    body ::
        Codec (Body (BodyCtx contract)) (IO b) (IO b) ->
        contract h LBS.ByteString ->
        contract h b
