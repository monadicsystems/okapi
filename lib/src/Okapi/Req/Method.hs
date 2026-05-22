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
) where

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

data Method a where
    Raw :: Method HTTP.Method
    StdMethod :: Method HTTP.StdMethod
    Method :: forall (m :: Kind.METHOD). Method (KnownMethod m)

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

known :: forall (m :: Kind.METHOD). Codec Method (KnownMethod m) (KnownMethod m)
known = Embed Method
