{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}

module Okapi.Req.Method (
    KnownMethod (..),
    Method,
    ParseError (..),
    parse,
    print,
    raw,
    std,
    known,
    knownMethodToStd,
    extractMethod,
    KGET,
    KPOST,
    KPUT,
    KDELETE,
) where

import Control.Applicative ((<|>))
import Network.HTTP.Types qualified as HTTP
import Okapi.Codec (Codec (..), ParseErrorOf, StateOf)
import Okapi.Codec qualified as Codec
import Okapi.Kind qualified as Kind (METHOD (..))
import Prelude hiding (print)

data KnownMethod (m :: Kind.METHOD) where
    GET :: KnownMethod Kind.GET
    POST :: KnownMethod Kind.POST
    PUT :: KnownMethod Kind.PUT
    DELETE :: KnownMethod Kind.DELETE

type KGET    = KnownMethod Kind.GET
type KPOST   = KnownMethod Kind.POST
type KPUT    = KnownMethod Kind.PUT
type KDELETE = KnownMethod Kind.DELETE

data Method a where
    Raw :: Method HTTP.Method
    StdMethod :: Method HTTP.StdMethod
    Method :: KnownMethod m -> Method (KnownMethod m)

data ParseError = ParseError

type instance StateOf Method = HTTP.Method
type instance ParseErrorOf Method = ParseError

parse :: Codec Method i o -> HTTP.Method -> (Either ParseError o, HTTP.Method)
parse = Codec.parser methodAlg
  where
    methodAlg = undefined

print :: Codec Method i o -> i -> HTTP.Method
print = Codec.printer methodPrinter
  where
    methodPrinter = undefined

raw :: Codec Method HTTP.Method HTTP.Method
raw = Embed Raw

std :: Codec Method HTTP.StdMethod HTTP.StdMethod
std = Embed StdMethod

known :: KnownMethod m -> Codec Method (KnownMethod m) (KnownMethod m)
known km = Embed (Method km)

knownMethodToStd :: KnownMethod m -> HTTP.StdMethod
knownMethodToStd GET    = HTTP.GET
knownMethodToStd POST   = HTTP.POST
knownMethodToStd PUT    = HTTP.PUT
knownMethodToStd DELETE = HTTP.DELETE

extractMethod :: Codec Method i o -> Maybe HTTP.StdMethod
extractMethod (Embed (Method km)) = Just (knownMethodToStd km)
extractMethod (Embed _)           = Nothing
extractMethod (FMap _ c)          = extractMethod c
extractMethod (LMap _ c)          = extractMethod c
extractMethod (Apply cf cx)       = extractMethod cf <|> extractMethod cx
extractMethod (Pure _)            = Nothing
