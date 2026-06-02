{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}

module Okapi.Res.Headers (
    Headers (..),
    ParseError (..),
    parse,
    print,
    raw,
    header,
    header',
    setCookie,
    setCookie',
) where

import Data.ByteString (ByteString)
import Data.Kind (Type)
import Network.HTTP.Types qualified as HTTP
import Okapi.Codec (Codec (..), ParseErrorOf, StateOf)
import Okapi.Codec qualified as Codec
import Okapi.Data (IsoHeaderData, IsoCookieData)
import Prelude hiding (print)

type Headers :: Type -> Type
data Headers a where
    Raw          :: Headers HTTP.ResponseHeaders
    Header       :: IsoHeaderData a => HTTP.HeaderName -> Headers a
    HeaderOpt    :: IsoHeaderData a => HTTP.HeaderName -> Headers (Maybe a)
    SetCookie    :: IsoCookieData a => ByteString -> Headers a
    SetCookieOpt :: IsoCookieData a => ByteString -> Headers (Maybe a)

data ParseError = ParseError

type instance StateOf Headers = HTTP.ResponseHeaders
type instance ParseErrorOf Headers = ParseError

parse :: Codec Headers i o -> HTTP.ResponseHeaders -> (Either ParseError o, HTTP.ResponseHeaders)
parse = Codec.parser resHeadersAlg
  where
    resHeadersAlg = undefined

print :: Codec Headers i o -> i -> HTTP.ResponseHeaders
print = Codec.printer resHeadersPrinter
  where
    resHeadersPrinter = undefined

raw :: Codec Headers HTTP.ResponseHeaders HTTP.ResponseHeaders
raw = Embed Raw

header :: IsoHeaderData a => HTTP.HeaderName -> Codec Headers a a
header key = Embed (Header key)

header' :: IsoHeaderData a => HTTP.HeaderName -> Codec Headers (Maybe a) (Maybe a)
header' key = Embed (HeaderOpt key)

setCookie :: IsoCookieData a => ByteString -> Codec Headers a a
setCookie name = Embed (SetCookie name)

setCookie' :: IsoCookieData a => ByteString -> Codec Headers (Maybe a) (Maybe a)
setCookie' name = Embed (SetCookieOpt name)
