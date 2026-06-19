{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Okapi.Protocol.Request.Query (
    Query (..),
    ArrayStyle (..),
    ParseError (..),
    parse,
    print,
    raw,
    param,
    param',
    flag,
    flag',
    list,
    list',
    deepObject,
    GQuery (..),
    queryCodec,
) where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS8
import Data.Kind (Type)
import Data.List (partition)
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding (decodeUtf8Lenient, encodeUtf8)
import GHC.Generics (D1, C1, S1, K1 (..), M1 (..), Rec0, (:*:) (..), Generic (..), Selector (..))
import Network.HTTP.Types qualified as HTTP
import Okapi.Codec (Codec (..), ParseErrorOf, StateOf)
import Okapi.Codec qualified as Codec
import Okapi.Data (IsoQueryData, parseQueryParam, toQueryParam)
import Okapi.Protocol.Request.Query.DeepObject (DeepObject)
import Okapi.Protocol.Request.Query.DeepObject qualified as DO
import Prelude hiding (print)

type Query :: Type -> Type
data Query a where
    Raw    :: Query HTTP.Query
    Param  :: IsoQueryData a => Text -> Query a
    Param' :: IsoQueryData a => Text -> Query (Maybe a)
    Flag   :: Text -> Query ()
    Flag'  :: Text -> Query (Maybe ())
    List   :: IsoQueryData a => ArrayStyle -> Text -> Query (NonEmpty a)
    List'  :: IsoQueryData a => ArrayStyle -> Text -> Query [a]
    DeepObj :: Text -> Codec DeepObject a a -> Query a

-- | How an array query parameter is serialized (OpenAPI @style@/@explode@ for arrays):
--   @Exploded@ → @a=1&a=2@ (form/explode); @CommaDelimited@ → @a=1,2@ (form);
--   @SpaceDelimited@ → @a=1%202@; @PipeDelimited@ → @a=1|2@.
data ArrayStyle = Exploded | CommaDelimited | SpaceDelimited | PipeDelimited deriving (Eq, Show)

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
    queryAlg (List style key) q =
        let (vals, rest) = collectList style key q
        in case traverse (parseQueryParam . decodeUtf8Lenient) vals of
            Left _   -> (Left ParseError, q)
            Right xs -> case NE.nonEmpty xs of
                Nothing  -> (Left ParseError, q)
                Just nel -> (Right nel, rest)
    queryAlg (List' style key) q =
        let (vals, rest) = collectList style key q
        in case traverse (parseQueryParam . decodeUtf8Lenient) vals of
            Left _   -> (Left ParseError, q)
            Right xs -> (Right xs, rest)
    queryAlg (DeepObj name c) q =
        let prefix = encodeUtf8 name <> "["
            (matched, rest) = partition (\(k, _) -> prefix `BS.isPrefixOf` k && "]" `BS.isSuffixOf` k) q
            fields = [ (innerKey prefix k, v) | (k, v) <- matched ]
        in case fst (DO.parse c fields) of
            Left _  -> (Left ParseError, q)
            Right x -> (Right x, rest)

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
    queryPrinter (List style key) nel  = renderList style key (NE.toList nel)
    queryPrinter (List' style key) xs  = renderList style key xs
    queryPrinter (DeepObj name c) a    =
        [ (encodeUtf8 name <> "[" <> k <> "]", v) | (k, v) <- DO.print c a ]

-- | Strip the @name[@ prefix and trailing @]@ from a deepObject key.
innerKey :: ByteString -> ByteString -> ByteString
innerKey prefix k = BS.dropEnd 1 (BS.drop (BS.length prefix) k)

-- | Collect the raw element values for an array parameter, plus the remaining query.
collectList :: ArrayStyle -> Text -> HTTP.Query -> ([ByteString], HTTP.Query)
collectList Exploded key q =
    let k = encodeUtf8 key
        (matched, rest) = partition (\(ik, _) -> ik == k) q
    in ([v | (_, Just v) <- matched], rest)
collectList style key q =
    let k = encodeUtf8 key
    in case partition (\(ik, _) -> ik == k) q of
        ([], _)                  -> ([], q)
        ((_, Nothing) : _, rest) -> ([], rest)
        ((_, Just v) : _, rest)  -> (filter (not . BS.null) (BS8.split (delim style) v), rest)

-- | Render an array parameter's elements per the chosen style.
renderList :: IsoQueryData a => ArrayStyle -> Text -> [a] -> HTTP.Query
renderList Exploded key xs = [(encodeUtf8 key, Just (enc x)) | x <- xs]
  where enc = encodeUtf8 . toQueryParam
renderList style key xs
    | null xs   = []
    | otherwise = [(encodeUtf8 key, Just (BS.intercalate (BS8.singleton (delim style)) (map enc xs)))]
  where enc = encodeUtf8 . toQueryParam

delim :: ArrayStyle -> Char
delim CommaDelimited = ','
delim SpaceDelimited = ' '
delim PipeDelimited  = '|'
delim Exploded       = ','  -- unused (Exploded never joins), present for totality

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

-- | Required array parameter (≥1 element) in the given serialization 'ArrayStyle'.
--   Parsing fails if the key is absent or no element parses.
list :: IsoQueryData a => ArrayStyle -> Text -> Codec Query (NonEmpty a) (NonEmpty a)
list style key = Lift (List style key)

-- | Optional/regular array parameter; an absent key yields @[]@.
list' :: IsoQueryData a => ArrayStyle -> Text -> Codec Query [a] [a]
list' style key = Lift (List' style key)

-- | A @deepObject@-encoded object parameter: its fields render as @name[field]=value@.
--   The second argument is a 'DeepObject' do-block of @field@/@field'@.
deepObject :: Text -> Codec DeepObject a a -> Codec Query a a
deepObject name c = Lift (DeepObj name c)


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
