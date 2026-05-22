{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}

module Okapi.Res.Headers (
    ResHeaders,
    ResHeadersParseError (..),
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

type ResHeaders :: Type -> Type
data ResHeaders a where
    Raw :: ResHeaders HTTP.ResponseHeaders
    Param :: IsoHttpApiData a => HTTP.HeaderName -> ResHeaders a
    Optional :: Codec ResHeaders a a -> ResHeaders (Maybe a)

data ResHeadersParseError = ResHeadersParseError

type instance StateOf ResHeaders = HTTP.ResponseHeaders
type instance ParseErrorOf ResHeaders = ResHeadersParseError

parse :: Codec ResHeaders i o -> HTTP.ResponseHeaders -> (Either ResHeadersParseError o, HTTP.ResponseHeaders)
parse = Codec.parser resHeadersAlg
  where
    resHeadersAlg = undefined

print :: Codec ResHeaders i o -> i -> HTTP.ResponseHeaders
print = Codec.printer resHeadersPrinter
  where
    resHeadersPrinter = undefined

raw :: Codec ResHeaders HTTP.ResponseHeaders HTTP.ResponseHeaders
raw = Embed Raw

param :: IsoHttpApiData a => HTTP.HeaderName -> Codec ResHeaders a a
param key = Embed (Param key)

optional :: Codec ResHeaders a a -> Codec ResHeaders (Maybe a) (Maybe a)
optional c = Embed (Optional c)
