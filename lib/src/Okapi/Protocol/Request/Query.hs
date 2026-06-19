{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Okapi.Protocol.Request.Query (
    Query (..),
    ParseError (..),
    parse,
    print,
    raw,
    param,
    param',
    flag,
    flag',
    GQuery (..),
    queryCodec,
) where

import Data.Kind (Type)
import Data.List (partition)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding (decodeUtf8Lenient, encodeUtf8)
import GHC.Generics (D1, C1, S1, K1 (..), M1 (..), Rec0, (:*:) (..), Generic (..), Selector (..))
import Network.HTTP.Types qualified as HTTP
import Okapi.Codec (Codec (..), ParseErrorOf, StateOf)
import Okapi.Codec qualified as Codec
import Okapi.Data (IsoQueryData, parseQueryParam, toQueryParam)
import Prelude hiding (print)

type Query :: Type -> Type
data Query a where
    Raw    :: Query HTTP.Query
    Param  :: IsoQueryData a => Text -> Query a
    Param' :: IsoQueryData a => Text -> Query (Maybe a)
    Flag   :: Text -> Query ()
    Flag'  :: Text -> Query (Maybe ())

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
            ([], _)       -> (Right Nothing, q)
            (_ : _, rest) -> (Right (Just ()), rest)

print :: Codec Query i o -> i -> HTTP.Query
print = Codec.printer queryPrinter
  where
    queryPrinter :: forall a. Query a -> a -> HTTP.Query
    queryPrinter Raw q              = q
    queryPrinter (Param key) x     = [(encodeUtf8 key, Just (encodeUtf8 (toQueryParam x)))]
    queryPrinter (Param' _) Nothing    = []
    queryPrinter (Param' key) (Just x) = [(encodeUtf8 key, Just (encodeUtf8 (toQueryParam x)))]
    queryPrinter (Flag key) ()         = [(encodeUtf8 key, Nothing)]
    queryPrinter (Flag' key) (Just ()) = [(encodeUtf8 key, Nothing)]
    queryPrinter (Flag' _) Nothing     = []

raw :: Codec Query HTTP.Query HTTP.Query
raw = Lift Raw

-- | Required query parameter; parsing fails if the key is absent.
param :: IsoQueryData a => Text -> Codec Query a a
param key = Lift (Param key)

-- | Optional query parameter; yields 'Nothing' when the key is absent.
param' :: IsoQueryData a => Text -> Codec Query (Maybe a) (Maybe a)
param' key = Lift (Param' key)

-- | Required flag parameter (bare key, no value); parsing fails if absent.
flag :: Text -> Codec Query () ()
flag key = Lift (Flag key)

-- | Optional flag parameter; yields 'Just ()' if the key is present, 'Nothing' otherwise.
flag' :: Text -> Codec Query (Maybe ()) (Maybe ())
flag' key = Lift (Flag' key)


-- ── Generic query deriving ────────────────────────────────────────────────────

class GQuery (f :: Type -> Type) where
    gQueryCodec :: Codec Query (f ()) (f ())

instance GQuery f => GQuery (D1 meta f) where
    gQueryCodec = FMap M1 $ LMap unM1 gQueryCodec

instance GQuery f => GQuery (C1 meta f) where
    gQueryCodec = FMap M1 $ LMap unM1 gQueryCodec

instance (GQuery f, GQuery g) => GQuery (f :*: g) where
    gQueryCodec =
        Apply
            (FMap (\l r -> l :*: r) (LMap (\(l :*: _) -> l) gQueryCodec))
            (LMap (\(_ :*: r) -> r) gQueryCodec)

instance (Selector s, IsoQueryData a) => GQuery (S1 s (Rec0 a)) where
    gQueryCodec =
        let key = Text.pack (selName (undefined :: S1 s (Rec0 a) ()))
        in FMap (M1 . K1) $ LMap (unK1 . unM1) $ Lift (Param key)

instance {-# OVERLAPPING #-} (Selector s, IsoQueryData a) => GQuery (S1 s (Rec0 (Maybe a))) where
    gQueryCodec =
        let key = Text.pack (selName (undefined :: S1 s (Rec0 (Maybe a)) ()))
        in FMap (M1 . K1) $ LMap (unK1 . unM1) $ Lift (Param' key)

instance {-# OVERLAPPING #-} Selector s => GQuery (S1 s (Rec0 ())) where
    gQueryCodec =
        let key = Text.pack (selName (undefined :: S1 s (Rec0 ()) ()))
        in FMap (M1 . K1) $ LMap (unK1 . unM1) $ Lift (Flag key)

instance {-# OVERLAPPING #-} Selector s => GQuery (S1 s (Rec0 (Maybe ()))) where
    gQueryCodec =
        let key = Text.pack (selName (undefined :: S1 s (Rec0 (Maybe ())) ()))
        in FMap (M1 . K1) $ LMap (unK1 . unM1) $ Lift (Flag' key)

-- | Build a 'Query' codec from a Generic record type.
--   Field types determine which 'Query' constructor is used:
--   @a@ → required param, @Maybe a@ → optional param,
--   @()@ → required flag, @Maybe ()@ → optional flag.
--   The field name becomes the query key.
queryCodec :: forall a. (Generic a, GQuery (Rep a)) => Codec Query a a
queryCodec = FMap (to @a) $ LMap (from @a) gQueryCodec
