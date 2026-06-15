{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}

module Okapi.Request.Query (
    Query (..),
    ParseError (..),
    parse,
    print,
    raw,
    param,
    param',
    flag,
    flag',
) where

import Data.Kind (Type)
import Data.List (partition)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8Lenient, encodeUtf8)
import Network.HTTP.Types qualified as HTTP
import Okapi.Codec (Codec (..), ParseErrorOf, StateOf)
import Okapi.Codec qualified as Codec
import Okapi.Data (IsoQueryData, parseQueryParam, toQueryParam)
import Prelude hiding (print)

type Query :: Type -> Type
data Query a where
    Raw      :: Query HTTP.Query
    Param    :: IsoQueryData a => Text -> Query a
    Param' :: IsoQueryData a => Text -> Query (Maybe a)
    Flag     :: Text -> Query ()
    Flag'  :: Text -> Query Bool

data ParseError = ParseError deriving (Eq, Show)

type instance StateOf Query = HTTP.Query
type instance ParseErrorOf Query = ParseError

parse :: Codec Query i o -> HTTP.Query -> (Either ParseError o, HTTP.Query)
parse = Codec.parser queryAlg
  where
    queryAlg :: forall a. Query a -> HTTP.Query -> (Either ParseError a, HTTP.Query)
    queryAlg Raw q = (Right q, [])
    queryAlg (Param key) q =
        case partition (\(k, _) -> k == encodeUtf8 key) q of
            ([], _)                   -> (Left ParseError, q)
            ((_, Nothing) : _, _)     -> (Left ParseError, q)
            ((_, Just v) : _, rest)   -> case parseQueryParam (decodeUtf8Lenient v) of
                Left _  -> (Left ParseError, q)
                Right x -> (Right x, rest)
    queryAlg (Param' key) q =
        case partition (\(k, _) -> k == encodeUtf8 key) q of
            ([], _)                   -> (Right Nothing, q)
            ((_, Nothing) : _, rest)  -> (Right Nothing, rest)
            ((_, Just v) : _, rest)   -> case parseQueryParam (decodeUtf8Lenient v) of
                Left _  -> (Right Nothing, rest)
                Right x -> (Right (Just x), rest)
    queryAlg (Flag key) q =
        case partition (\(k, _) -> k == encodeUtf8 key) q of
            ([], _)       -> (Left ParseError, q)
            (_ : _, rest) -> (Right (), rest)
    queryAlg (Flag' key) q =
        case partition (\(k, _) -> k == encodeUtf8 key) q of
            ([], _)       -> (Right False, q)
            (_ : _, rest) -> (Right True, rest)

print :: Codec Query i o -> i -> HTTP.Query
print = Codec.printer queryPrinter
  where
    queryPrinter :: forall a. Query a -> a -> HTTP.Query
    queryPrinter Raw q                   = q
    queryPrinter (Param key) x           = [(encodeUtf8 key, Just (encodeUtf8 (toQueryParam x)))]
    queryPrinter (Param' _) Nothing    = []
    queryPrinter (Param' key) (Just x) = [(encodeUtf8 key, Just (encodeUtf8 (toQueryParam x)))]
    queryPrinter (Flag key) ()           = [(encodeUtf8 key, Nothing)]
    queryPrinter (Flag' key) True      = [(encodeUtf8 key, Nothing)]
    queryPrinter (Flag' _) False       = []

raw :: Codec Query HTTP.Query HTTP.Query
raw = Embed Raw

-- | Required query parameter; parsing fails if the key is absent.
param :: IsoQueryData a => Text -> Codec Query a a
param key = Embed (Param key)

-- | Optional query parameter; yields 'Nothing' when the key is absent.
param' :: IsoQueryData a => Text -> Codec Query (Maybe a) (Maybe a)
param' key = Embed (Param' key)

-- | Required flag parameter (bare key, no value); parsing fails if absent.
flag :: Text -> Codec Query () ()
flag key = Embed (Flag key)

-- | Optional flag parameter; yields 'True' if the key is present, 'False' otherwise.
flag' :: Text -> Codec Query Bool Bool
flag' key = Embed (Flag' key)
