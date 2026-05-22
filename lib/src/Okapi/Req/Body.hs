{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}

module Okapi.Req.Body (
    ReqBody,
    IsoJson,
    BodyParseError (..),
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

type ReqBody :: Type -> Type
data ReqBody a where
    Raw :: ReqBody (IO LBS.ByteString)
    Json :: IsoJson a => ReqBody (IO a)

data BodyParseError = BodyParseError

type instance StateOf ReqBody = LBS.ByteString
type instance ParseErrorOf ReqBody = BodyParseError

parse :: Codec ReqBody i o -> LBS.ByteString -> (Either BodyParseError o, LBS.ByteString)
parse = Codec.parser bodyAlg
  where
    bodyAlg = undefined

print :: Codec ReqBody i o -> i -> LBS.ByteString
print = Codec.printer bodyPrinter
  where
    bodyPrinter = undefined

raw :: Codec ReqBody (IO LBS.ByteString) (IO LBS.ByteString)
raw = Embed Raw

json :: IsoJson a => Codec ReqBody (IO a) (IO a)
json = Embed Json
