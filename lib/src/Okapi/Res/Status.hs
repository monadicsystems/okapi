{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}

module Okapi.Res.Status (
    KnownStatus (..),
    Status,
    ParseError (..),
    parse,
    print,
    raw,
    known,
    knownStatusToHTTP,
    extractStatus,
    KS200,
    KS404,
    KS500,
) where

import Control.Applicative ((<|>))
import Data.Kind (Type)
import Network.HTTP.Types qualified as HTTP
import Okapi.Codec (Codec (..), ParseErrorOf, StateOf)
import Okapi.Codec qualified as Codec
import Okapi.Kind qualified as Kind (STATUS (..))
import Prelude hiding (print)

data KnownStatus (s :: Kind.STATUS) where
    S200 :: KnownStatus Kind.S200
    S404 :: KnownStatus Kind.S404
    S500 :: KnownStatus Kind.S500

type KS200 = KnownStatus Kind.S200
type KS404 = KnownStatus Kind.S404
type KS500 = KnownStatus Kind.S500

instance Num (KnownStatus Kind.S200) where
    fromInteger 200 = S200
    fromInteger _ = error "KnownStatus S200: expected 200"
    S200 + S200 = S200
    S200 * S200 = S200
    abs = id
    signum _ = S200
    negate = id

instance Num (KnownStatus Kind.S404) where
    fromInteger 404 = S404
    fromInteger _ = error "KnownStatus S404: expected 404"
    S404 + S404 = S404
    S404 * S404 = S404
    abs = id
    signum _ = S404
    negate = id

instance Num (KnownStatus Kind.S500) where
    fromInteger 500 = S500
    fromInteger _ = error "KnownStatus S500: expected 500"
    S500 + S500 = S500
    S500 * S500 = S500
    abs = id
    signum _ = S500
    negate = id

type Status :: Type -> Type
data Status a where
    Raw :: Status HTTP.Status
    Status :: KnownStatus s -> Status (KnownStatus s)

data ParseError = ParseError

type instance StateOf Status = HTTP.Status
type instance ParseErrorOf Status = ParseError

parse :: Codec Status i o -> HTTP.Status -> (Either ParseError o, HTTP.Status)
parse = Codec.parser statusAlg
  where
    statusAlg = undefined

-- HTTP.Status is not a Monoid so Codec.printer cannot be used here.
print :: Codec Status i o -> i -> HTTP.Status
print = undefined

raw :: Codec Status HTTP.Status HTTP.Status
raw = Embed Raw

known :: KnownStatus s -> Codec Status (KnownStatus s) (KnownStatus s)
known ks = Embed (Status ks)

knownStatusToHTTP :: KnownStatus s -> HTTP.Status
knownStatusToHTTP S200 = HTTP.status200
knownStatusToHTTP S404 = HTTP.status404
knownStatusToHTTP S500 = HTTP.status500

extractStatus :: Codec Status i o -> Maybe HTTP.Status
extractStatus (Embed (Status ks)) = Just (knownStatusToHTTP ks)
extractStatus (Embed Raw)         = Nothing
extractStatus (FMap _ c)          = extractStatus c
extractStatus (LMap _ c)          = extractStatus c
extractStatus (Apply cf cx)       = extractStatus cf <|> extractStatus cx
extractStatus (Pure _)            = Nothing
