{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}

module Okapi.Req.Headers (
    Headers (..),
    ParseError (..),
    parse,
    print,
    raw,
    param,
    optional,
) where

import Data.Kind (Type)
import Network.HTTP.Types qualified as HTTP
import Okapi.Codec (Codec (..), ParseErrorOf, StateOf)
import Okapi.Codec qualified as Codec
import Web.HttpApiData qualified as HTTP.Api
import Prelude hiding (print)

type IsoHttpApiData a = (HTTP.Api.FromHttpApiData a, HTTP.Api.ToHttpApiData a)

type Headers :: Type -> Type
data Headers a where
    Raw :: Headers HTTP.RequestHeaders
    Param :: IsoHttpApiData a => HTTP.HeaderName -> Headers a
    Optional :: Codec Headers a a -> Headers (Maybe a)

data ParseError = ParseError

type instance StateOf Headers = HTTP.RequestHeaders
type instance ParseErrorOf Headers = ParseError

parse :: Codec Headers i o -> HTTP.RequestHeaders -> (Either ParseError o, HTTP.RequestHeaders)
parse = Codec.parser headersAlg
  where
    headersAlg = undefined

print :: Codec Headers i o -> i -> HTTP.RequestHeaders
print = Codec.printer headersPrinter
  where
    headersPrinter = undefined

raw :: Codec Headers HTTP.RequestHeaders HTTP.RequestHeaders
raw = Embed Raw

param :: IsoHttpApiData a => HTTP.HeaderName -> Codec Headers a a
param key = Embed (Param key)

optional :: Codec Headers a a -> Codec Headers (Maybe a) (Maybe a)
optional c = Embed (Optional c)
