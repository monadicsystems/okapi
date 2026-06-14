{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}

module Okapi.Request.Method (
    KnownMethod (..),
    Method,
    ParseError (..),
    parse,
    print,
    raw,
    known,
    knownMethodToStd,
    extractMethod,
    GET,
    POST,
    PUT,
    DELETE,
) where

import Control.Applicative ((<|>))
import GHC.TypeLits (Symbol)
import Network.HTTP.Types qualified as HTTP
import Okapi.Codec (Codec (..), ParseErrorOf, StateOf)
import Okapi.Codec qualified as Codec
import Prelude hiding (print)

data KnownMethod (m :: Symbol) where
    GET    :: KnownMethod "GET"
    POST   :: KnownMethod "POST"
    PUT    :: KnownMethod "PUT"
    DELETE :: KnownMethod "DELETE"

deriving instance Eq   (KnownMethod m)
deriving instance Show (KnownMethod m)

type GET    = KnownMethod "GET"
type POST   = KnownMethod "POST"
type PUT    = KnownMethod "PUT"
type DELETE = KnownMethod "DELETE"

data Method a where
    Raw    :: Method HTTP.Method
    Method :: KnownMethod m -> Method (KnownMethod m)

data ParseError = ParseError deriving (Eq, Show)

type instance StateOf Method = HTTP.Method
type instance ParseErrorOf Method = ParseError

parse :: Codec Method i o -> HTTP.Method -> (Either ParseError o, HTTP.Method)
parse = Codec.parser methodAlg
  where
    methodAlg :: forall a. Method a -> HTTP.Method -> (Either ParseError a, HTTP.Method)
    methodAlg Raw         m = (Right m, m)
    methodAlg (Method km) m
        | m == HTTP.renderStdMethod (knownMethodToStd km) = (Right km, m)
        | otherwise                                       = (Left ParseError, m)

print :: Codec Method i o -> i -> HTTP.Method
print = Codec.printer methodPrinter
  where
    methodPrinter :: forall a. Method a -> a -> HTTP.Method
    methodPrinter Raw         m  = m
    methodPrinter (Method km) _  = HTTP.renderStdMethod (knownMethodToStd km)

raw :: Codec Method HTTP.Method HTTP.Method
raw = Embed Raw

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
