
module Okapi.HTTP.Request.Method (
    KnownMethod (..),
    Method (..),
    ParseError (..),
    parse,
    print,
    raw,
    method,
    knownMethodToStd,
    extractMethod,
    GET,
    POST,
    PUT,
    DELETE,
) where

import GHC.TypeLits (Symbol)
import Network.HTTP.Types qualified as HTTP
import Okapi.Tree (Failure, Context)
import Prelude hiding (print)

data KnownMethod (m :: Symbol) where
    GET :: KnownMethod "GET"
    POST :: KnownMethod "POST"
    PUT :: KnownMethod "PUT"
    DELETE :: KnownMethod "DELETE"

deriving instance Eq (KnownMethod m)
deriving instance Show (KnownMethod m)

type GET = KnownMethod "GET"
type POST = KnownMethod "POST"
type PUT = KnownMethod "PUT"
type DELETE = KnownMethod "DELETE"

data Method a where
    Raw :: Method HTTP.Method
    Method :: KnownMethod m -> Method (KnownMethod m)

data ParseError = ParseError deriving (Eq, Show)

type instance Context Method = HTTP.Method
type instance Failure Method = ParseError

parse :: Method method -> HTTP.Method -> Either ParseError method
parse Raw m = Right m
parse (Method km) m
    | m == HTTP.renderStdMethod (knownMethodToStd km) = Right km
    | otherwise = Left ParseError

print :: Method method -> method -> HTTP.Method
print Raw m = m
print (Method km) _ = HTTP.renderStdMethod (knownMethodToStd km)

raw :: Method HTTP.Method
raw = Raw

method :: KnownMethod m -> Method (KnownMethod m)
method km = (Method km)

knownMethodToStd :: KnownMethod m -> HTTP.StdMethod
knownMethodToStd GET = HTTP.GET
knownMethodToStd POST = HTTP.POST
knownMethodToStd PUT = HTTP.PUT
knownMethodToStd DELETE = HTTP.DELETE

extractMethod :: Method (KnownMethod m) -> HTTP.StdMethod
extractMethod (Method km) = knownMethodToStd km

