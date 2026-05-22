{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}

module Okapi.Req.Headers (
    ReqHeaders,
    HeadersParseError (..),
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

type ReqHeaders :: Type -> Type
data ReqHeaders a where
    Raw :: ReqHeaders HTTP.RequestHeaders
    Param :: IsoHttpApiData a => HTTP.HeaderName -> ReqHeaders a
    Optional :: Codec ReqHeaders a a -> ReqHeaders (Maybe a)

data HeadersParseError = HeadersParseError

type instance StateOf ReqHeaders = HTTP.RequestHeaders
type instance ParseErrorOf ReqHeaders = HeadersParseError

parse :: Codec ReqHeaders i o -> HTTP.RequestHeaders -> (Either HeadersParseError o, HTTP.RequestHeaders)
parse = Codec.parser headersAlg
  where
    headersAlg = undefined

print :: Codec ReqHeaders i o -> i -> HTTP.RequestHeaders
print = Codec.printer headersPrinter
  where
    headersPrinter = undefined

raw :: Codec ReqHeaders HTTP.RequestHeaders HTTP.RequestHeaders
raw = Embed Raw

param :: IsoHttpApiData a => HTTP.HeaderName -> Codec ReqHeaders a a
param key = Embed (Param key)

optional :: Codec ReqHeaders a a -> Codec ReqHeaders (Maybe a) (Maybe a)
optional c = Embed (Optional c)
