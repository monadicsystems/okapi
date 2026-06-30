{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}

module Okapi.HTTP.Response.Status (
    KnownStatus (..),
    Status (..),
    ParseError (..),
    parse,
    print,
    raw,
    status,
    knownStatusToHTTP,
    extractStatus,
    S200,
    S201,
    S204,
    S404,
    S500,
) where

import Data.Kind (Type)
import GHC.TypeLits (Nat)
import Network.HTTP.Types qualified as HTTP
import Okapi.Leaf (ErrorOf, StateOf)
import Prelude hiding (print)

data KnownStatus (s :: Nat) where
    S200 :: KnownStatus 200
    S201 :: KnownStatus 201
    S204 :: KnownStatus 204
    S404 :: KnownStatus 404
    S500 :: KnownStatus 500

deriving instance Eq   (KnownStatus s)
deriving instance Show (KnownStatus s)

type S200 = KnownStatus 200
type S201 = KnownStatus 201
type S204 = KnownStatus 204
type S404 = KnownStatus 404
type S500 = KnownStatus 500

instance Num (KnownStatus 200) where
    fromInteger 200 = S200
    fromInteger _ = error "KnownStatus 200: expected 200"
    S200 + S200 = S200
    S200 * S200 = S200
    abs = id; signum _ = S200; negate = id

instance Num (KnownStatus 201) where
    fromInteger 201 = S201
    fromInteger _ = error "KnownStatus 201: expected 201"
    S201 + S201 = S201
    S201 * S201 = S201
    abs = id; signum _ = S201; negate = id

instance Num (KnownStatus 204) where
    fromInteger 204 = S204
    fromInteger _ = error "KnownStatus 204: expected 204"
    S204 + S204 = S204
    S204 * S204 = S204
    abs = id; signum _ = S204; negate = id

instance Num (KnownStatus 404) where
    fromInteger 404 = S404
    fromInteger _ = error "KnownStatus 404: expected 404"
    S404 + S404 = S404
    S404 * S404 = S404
    abs = id; signum _ = S404; negate = id

instance Num (KnownStatus 500) where
    fromInteger 500 = S500
    fromInteger _ = error "KnownStatus 500: expected 500"
    S500 + S500 = S500
    S500 * S500 = S500
    abs = id; signum _ = S500; negate = id

type Status :: Type -> Type
data Status a where
    Raw    :: Status HTTP.Status
    Status :: KnownStatus s -> Status (KnownStatus s)

data ParseError = ParseError deriving (Eq, Show)

type instance StateOf Status = HTTP.Status
type instance ErrorOf Status = ParseError

parse :: Status status -> HTTP.Status -> Either ParseError status
parse Raw         s = Right s
parse (Status ks) s
    | s == knownStatusToHTTP ks = Right ks
    | otherwise                 = Left ParseError

print :: Status status -> status -> HTTP.Status
print Raw         s  = s
print (Status ks) _  = knownStatusToHTTP ks

raw :: Status HTTP.Status
raw = Raw

status :: KnownStatus s -> Status (KnownStatus s)
status = Status

knownStatusToHTTP :: KnownStatus s -> HTTP.Status
knownStatusToHTTP S200 = HTTP.status200
knownStatusToHTTP S201 = HTTP.status201
knownStatusToHTTP S204 = HTTP.status204
knownStatusToHTTP S404 = HTTP.status404
knownStatusToHTTP S500 = HTTP.status500

extractStatus :: Status (KnownStatus s) -> HTTP.Status
extractStatus (Status ks) = knownStatusToHTTP ks
