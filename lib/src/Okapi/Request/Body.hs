{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}

module Okapi.Request.Body (
    Body (..),
    IsoJson,
    ParseError (..),
    parse,
    printM,
    raw,
    json,
) where

import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as LBS
import Data.Kind (Type)
import Data.OpenApi (ToSchema)
import Okapi.Codec (Codec (..), ParseErrorOf, StateOf)
import Okapi.Codec qualified as Codec

type IsoJson a = (Aeson.FromJSON a, Aeson.ToJSON a, ToSchema a)

type Body :: Type -> Type
data Body a where
    Raw  :: Body (IO LBS.ByteString)
    Json :: IsoJson a => Body (IO a)

data ParseError = ParseError deriving (Eq, Show)

type instance StateOf Body = LBS.ByteString
type instance ParseErrorOf Body = ParseError

parse :: Codec Body i o -> LBS.ByteString -> (Either ParseError o, LBS.ByteString)
parse = Codec.parser bodyAlg
  where
    bodyAlg :: forall a. Body a -> LBS.ByteString -> (Either ParseError a, LBS.ByteString)
    bodyAlg Raw  bs = (Right (pure bs), LBS.empty)
    bodyAlg Json bs = case Aeson.eitherDecode bs of
        Left _  -> (Left ParseError, bs)
        Right x -> (Right (pure x), LBS.empty)

printM :: Codec Body i o -> i -> IO LBS.ByteString
printM = go
  where
    go :: forall i' o'. Codec Body i' o' -> i' -> IO LBS.ByteString
    go (Pure _)      _ = pure mempty
    go (FMap _ c)    i = go c i
    go (LMap f c)    i = go c (f i)
    go (Apply cf cx) i = liftA2 (<>) (go cf i) (go cx i)
    go (Embed Raw)   ioLbs = ioLbs
    go (Embed Json)  ioA   = Aeson.encode <$> ioA

raw :: Codec Body (IO LBS.ByteString) (IO LBS.ByteString)
raw = Embed Raw

json :: IsoJson a => Codec Body (IO a) (IO a)
json = Embed Json
