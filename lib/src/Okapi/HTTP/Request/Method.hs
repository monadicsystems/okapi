module Okapi.HTTP.Request.Method (
    KnownMethod (..),
    Method (..),
    Base,
    ParseError (..),
    parse,
    print,
    base,
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

{- $setup
>>> import Okapi.HTTP.Request.Method qualified as Method
>>> import Network.HTTP.Types qualified as Types
>>> import Okapi.HTTP.Tree (leafPrintParse, leafParsePrint)
>>> import Test.QuickCheck.Instances ()
-}

data KnownMethod (m :: Symbol) where
    Get :: GET
    Post :: POST
    Put :: PUT
    Delete :: DELETE
    Patch :: PATCH
    Head :: HEAD
    Options :: OPTIONS
    Connect :: CONNECT
    Trace :: TRACE

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
    Base :: Method Base
    Method :: KnownMethod m -> Method (KnownMethod m)

-- | What 'raw' decodes\/encodes to — the maximally unconstrained method slot.
type Base = Types.Method

data ParseError = ParseError deriving (Eq, Show)

parse :: Method method -> Types.Method -> Either ParseError method
parse Base m = Right m
parse (Method km) m
    | m == Types.renderStdMethod (knownMethodToStd km) = Right km
    | otherwise = Left ParseError

print :: Method method -> method -> Types.Method
print Base m = m
print (Method km) _ = Types.renderStdMethod (knownMethodToStd km)

{- | Pass the raw HTTP method straight through, unconstrained.

>>> parse base "PATCH"
Right "PATCH"
>>> Method.print base "PATCH"
"PATCH"

prop> leafPrintParse (parse base) (Method.print base) (m :: Types.Method)
prop> leafParsePrint (parse base) (Method.print base) (m :: Types.Method)
-}
base :: Method Types.Method
base = Base

{- | Match against a statically known HTTP method.

>>> parse (method Get) "GET"
Right Get
>>> parse (method Get) "POST"
Left ParseError
>>> Method.print (method Get) Get
"GET"
>>> parse (method Post) "POST"
Right Post
>>> parse (method Put) "PUT"
Right Put
>>> parse (method Delete) "DELETE"
Right Delete
>>> parse (method Patch) "PATCH"
Right Patch
>>> parse (method Head) "HEAD"
Right Head
>>> parse (method Options) "OPTIONS"
Right Options
>>> parse (method Connect) "CONNECT"
Right Connect
>>> parse (method Trace) "TRACE"
Right Trace
-}
method :: KnownMethod m -> Method (KnownMethod m)
method km = (Method km)

knownMethodToStd :: KnownMethod m -> Types.StdMethod
knownMethodToStd Get = Types.GET
knownMethodToStd Post = Types.POST
knownMethodToStd Put = Types.PUT
knownMethodToStd Delete = Types.DELETE
knownMethodToStd Patch = Types.PATCH
knownMethodToStd Head = Types.HEAD
knownMethodToStd Options = Types.OPTIONS
knownMethodToStd Connect = Types.CONNECT
knownMethodToStd Trace = Types.TRACE

extractMethod :: Method (KnownMethod m) -> Types.StdMethod
extractMethod (Method km) = knownMethodToStd km
