{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}

module Okapi.Req.Headers (
    Headers (..),
    ParseError (..),
    parse,
    print,
    raw,
    header,
    headerOpt,
) where

import Data.Kind (Type)
import Network.HTTP.Types qualified as HTTP
import Okapi.Codec (Codec (..), ParseErrorOf, StateOf)
import Okapi.Codec qualified as Codec
import Okapi.Data (IsoHeaderData)
import Prelude hiding (print)

type Headers :: Type -> Type
data Headers a where
    Raw       :: Headers HTTP.RequestHeaders
    Header    :: IsoHeaderData a => HTTP.HeaderName -> Headers a
    HeaderOpt :: IsoHeaderData a => HTTP.HeaderName -> Headers (Maybe a)

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

header :: IsoHeaderData a => HTTP.HeaderName -> Codec Headers a a
header key = Embed (Header key)

headerOpt :: IsoHeaderData a => HTTP.HeaderName -> Codec Headers (Maybe a) (Maybe a)
headerOpt key = Embed (HeaderOpt key)
