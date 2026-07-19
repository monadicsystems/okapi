
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
import Network.HTTP.Types qualified as Types
import Prelude hiding (print)

-- $setup
-- >>> import Okapi.HTTP.Request.Method qualified as Method
-- >>> import Network.HTTP.Types qualified as Types
-- >>> import Okapi.HTTP.Tree (leafPrintParse, leafParsePrint)
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
    Raw :: Method Types.Method
    Method :: KnownMethod m -> Method (KnownMethod m)

data ParseError = ParseError deriving (Eq, Show)

parse :: Method method -> Types.Method -> Either ParseError method
parse Raw m = Right m
parse (Method km) m
    | m == Types.renderStdMethod (knownMethodToStd km) = Right km
    | otherwise = Left ParseError

print :: Method method -> method -> Types.Method
print Raw m = m
print (Method km) _ = Types.renderStdMethod (knownMethodToStd km)

-- | Pass the raw HTTP method straight through, unconstrained.
--
-- >>> parse raw "PATCH"
-- Right "PATCH"
-- >>> Method.print raw "PATCH"
-- "PATCH"
--
-- prop> leafPrintParse (parse raw) (Method.print raw) (m :: Types.Method)
-- prop> leafParsePrint (parse raw) (Method.print raw) (m :: Types.Method)
raw :: Method Types.Method
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

knownMethodToStd :: KnownMethod m -> Types.StdMethod
knownMethodToStd GET = Types.GET
knownMethodToStd POST = Types.POST
knownMethodToStd PUT = Types.PUT
knownMethodToStd DELETE = Types.DELETE
knownMethodToStd PATCH = Types.PATCH
knownMethodToStd HEAD = Types.HEAD
knownMethodToStd OPTIONS = Types.OPTIONS
knownMethodToStd CONNECT = Types.CONNECT
knownMethodToStd TRACE = Types.TRACE

extractMethod :: Method (KnownMethod m) -> Types.StdMethod
extractMethod (Method km) = knownMethodToStd km

