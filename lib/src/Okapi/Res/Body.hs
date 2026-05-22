{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}

module Okapi.Res.Body (
    ResBody,
    IsoJson,
    ResBodyParseError (..),
    parse,
    print,
    raw,
    json,
) where

import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as LBS
import Data.Kind (Type)
import Okapi.Codec (Codec (..), ParseErrorOf, StateOf)
import Okapi.Codec qualified as Codec
import Prelude hiding (print)

type IsoJson a = (Aeson.FromJSON a, Aeson.ToJSON a)

type ResBody :: Type -> Type
data ResBody a where
    Raw :: ResBody (IO LBS.ByteString)
    Json :: IsoJson a => ResBody (IO a)

data ResBodyParseError = ResBodyParseError

type instance StateOf ResBody = LBS.ByteString
type instance ParseErrorOf ResBody = ResBodyParseError

parse :: Codec ResBody i o -> LBS.ByteString -> (Either ResBodyParseError o, LBS.ByteString)
parse = Codec.parser resBodyAlg
  where
    resBodyAlg = undefined

print :: Codec ResBody i o -> i -> LBS.ByteString
print = Codec.printer resBodyPrinter
  where
    resBodyPrinter = undefined

raw :: Codec ResBody (IO LBS.ByteString) (IO LBS.ByteString)
raw = Embed Raw

json :: IsoJson a => Codec ResBody (IO a) (IO a)
json = Embed Json
