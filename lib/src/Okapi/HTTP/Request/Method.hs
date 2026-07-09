
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
    PATCH,
    HEAD,
    OPTIONS,
    CONNECT,
    TRACE,
) where

import GHC.TypeLits (Symbol)
import Network.HTTP.Types qualified as HTTP
import Okapi.Tree (Failure, Context)
import Prelude hiding (print)

-- $setup
-- >>> import Okapi.HTTP.Request.Method qualified as Method
-- >>> import Network.HTTP.Types qualified as HTTP
-- >>> import Okapi.Tree (leafPrintParse, leafParsePrint)
-- >>> import Test.QuickCheck.Instances ()

data KnownMethod (m :: Symbol) where
    GET :: KnownMethod "GET"
    POST :: KnownMethod "POST"
    PUT :: KnownMethod "PUT"
    DELETE :: KnownMethod "DELETE"
    PATCH :: KnownMethod "PATCH"
    HEAD :: KnownMethod "HEAD"
    OPTIONS :: KnownMethod "OPTIONS"
    CONNECT :: KnownMethod "CONNECT"
    TRACE :: KnownMethod "TRACE"

deriving instance Eq (KnownMethod m)
deriving instance Show (KnownMethod m)

type GET = KnownMethod "GET"
type POST = KnownMethod "POST"
type PUT = KnownMethod "PUT"
type DELETE = KnownMethod "DELETE"
type PATCH = KnownMethod "PATCH"
type HEAD = KnownMethod "HEAD"
type OPTIONS = KnownMethod "OPTIONS"
type CONNECT = KnownMethod "CONNECT"
type TRACE = KnownMethod "TRACE"

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

-- | Pass the raw HTTP method straight through, unconstrained.
--
-- >>> parse raw "PATCH"
-- Right "PATCH"
-- >>> Method.print raw "PATCH"
-- "PATCH"
--
-- prop> leafPrintParse (parse raw) (Method.print raw) (m :: HTTP.Method)
-- prop> leafParsePrint (parse raw) (Method.print raw) (m :: HTTP.Method)
raw :: Method HTTP.Method
raw = Raw

-- | Match against a statically known HTTP method.
--
-- >>> parse (method GET) "GET"
-- Right GET
-- >>> parse (method GET) "POST"
-- Left ParseError
-- >>> Method.print (method GET) GET
-- "GET"
-- >>> parse (method POST) "POST"
-- Right POST
-- >>> parse (method PUT) "PUT"
-- Right PUT
-- >>> parse (method DELETE) "DELETE"
-- Right DELETE
-- >>> parse (method PATCH) "PATCH"
-- Right PATCH
-- >>> parse (method HEAD) "HEAD"
-- Right HEAD
-- >>> parse (method OPTIONS) "OPTIONS"
-- Right OPTIONS
-- >>> parse (method CONNECT) "CONNECT"
-- Right CONNECT
-- >>> parse (method TRACE) "TRACE"
-- Right TRACE
method :: KnownMethod m -> Method (KnownMethod m)
method km = (Method km)

knownMethodToStd :: KnownMethod m -> HTTP.StdMethod
knownMethodToStd GET = HTTP.GET
knownMethodToStd POST = HTTP.POST
knownMethodToStd PUT = HTTP.PUT
knownMethodToStd DELETE = HTTP.DELETE
knownMethodToStd PATCH = HTTP.PATCH
knownMethodToStd HEAD = HTTP.HEAD
knownMethodToStd OPTIONS = HTTP.OPTIONS
knownMethodToStd CONNECT = HTTP.CONNECT
knownMethodToStd TRACE = HTTP.TRACE

extractMethod :: Method (KnownMethod m) -> HTTP.StdMethod
extractMethod (Method km) = knownMethodToStd km

