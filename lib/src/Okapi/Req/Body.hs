{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}

module Okapi.Req.Body (
    Body (..),
    IsoJson,
    ParseError (..),
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

type Body :: Type -> Type
data Body a where
    Raw :: Body (IO LBS.ByteString)
    Json :: IsoJson a => Body (IO a)

data ParseError = ParseError

type instance StateOf Body = LBS.ByteString
type instance ParseErrorOf Body = ParseError

parse :: Codec Body i o -> LBS.ByteString -> (Either ParseError o, LBS.ByteString)
parse = Codec.parser bodyAlg
  where
    bodyAlg = undefined

print :: Codec Body i o -> i -> LBS.ByteString
print = Codec.printer bodyPrinter
  where
    bodyPrinter = undefined

raw :: Codec Body (IO LBS.ByteString) (IO LBS.ByteString)
raw = Embed Raw

json :: IsoJson a => Codec Body (IO a) (IO a)
json = Embed Json
