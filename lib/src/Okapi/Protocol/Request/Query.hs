{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedRecordDot #-}
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
    param_,
    flag,
    flag',
    list,
    list',
    GQuery (..),
    queryCodec,
) where

import Data.Bifunctor (first)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS8
import Data.Int (Int16, Int32, Int64)
import Data.Kind (Type)
import Data.List (partition)
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding (decodeUtf8Lenient, encodeUtf8)
import Data.Time (Day, UTCTime)
import Data.UUID (UUID)
import GHC.Generics (C1, D1, Generic (..), K1 (..), M1 (..), Rec0, S1, Selector (..), (:*:) (..))
import Network.HTTP.Types qualified as HTTP
import Okapi.Codec (Codec (..), ParseErrorOf, PartOf, StateOf)
import Okapi.Codec qualified as Codec
import Okapi.Data (Data (..), Info (..), Iso (..))
import Prelude hiding (print)
import Web.HttpApiData (parseQueryParam, toQueryParam)

type Query :: Type -> Type
data Query a where
    Raw :: Query HTTP.Query
    Param :: Text -> Iso Query a -> Query a
    Param' :: Text -> Iso Query a -> Query (Maybe a)
    Param_ :: Text -> Iso Query a -> a -> Query ()
    Flag :: Text -> Query ()
    Flag' :: Text -> Query Bool
    List :: ArrayStyle -> Text -> Iso Query a -> Query (NonEmpty a)
    List' :: ArrayStyle -> Text -> Iso Query a -> Query [a]

data ArrayStyle = Exploded | CommaDelimited | SpaceDelimited | PipeDelimited deriving (Eq, Show)

data ParseError = ParseError deriving (Eq, Show)

type instance StateOf Query = HTTP.Query
type instance ParseErrorOf Query = ParseError
type instance PartOf Query = Text

parse :: Codec Query i o -> HTTP.Query -> (Either ParseError o, HTTP.Query)
parse = Codec.parser queryAlg
  where
    queryAlg :: forall a. Query a -> HTTP.Query -> (Either ParseError a, HTTP.Query)
    queryAlg Raw q = (Right q, [])
    queryAlg (Param key vIso) q =
        case partition (\(k, _) -> k == encodeUtf8 key) q of
            ([], _) -> (Left ParseError, q)
            ((_, Nothing) : _, _) -> (Left ParseError, q)
            ((_, Just v) : _, rest) -> case vIso.decode (decodeUtf8Lenient v) of
                Left e -> (Left e, q)
                Right x -> (Right x, rest)
    queryAlg (Param' key vIso) q =
        case partition (\(k, _) -> k == encodeUtf8 key) q of
            ([], _) -> (Right Nothing, q)
            ((_, Nothing) : _, rest) -> (Right Nothing, rest)
            ((_, Just v) : _, rest) -> case vIso.decode (decodeUtf8Lenient v) of
                Left _ -> (Right Nothing, rest)
                Right x -> (Right (Just x), rest)
    queryAlg (Flag key) q =
        case partition (\(k, _) -> k == encodeUtf8 key) q of
            ([], _) -> (Left ParseError, q)
            (_ : _, rest) -> (Right (), rest)
    queryAlg (Param_ key vIso x) q =
        case partition (\(k, _) -> k == encodeUtf8 key) q of
            ((_, Just v) : _, rest) | decodeUtf8Lenient v == vIso.encode x -> (Right (), rest)
            _ -> (Left ParseError, q)
    queryAlg (Flag' key) q =
        case partition (\(k, _) -> k == encodeUtf8 key) q of
            ([], _) -> (Right False, q)
            (_ : _, rest) -> (Right True, rest)
    queryAlg (List style key vIso) q =
        let (vals, rest) = collectList style key q
         in case traverse (vIso.decode . decodeUtf8Lenient) vals of
                Left _ -> (Left ParseError, q)
                Right xs -> case NE.nonEmpty xs of
                    Nothing -> (Left ParseError, q)
                    Just nel -> (Right nel, rest)
    queryAlg (List' style key vIso) q =
        let (vals, rest) = collectList style key q
         in case traverse (vIso.decode . decodeUtf8Lenient) vals of
                Left _ -> (Left ParseError, q)
                Right xs -> (Right xs, rest)

print :: Codec Query i o -> i -> HTTP.Query
print = Codec.printer queryPrinter
  where
    queryPrinter :: forall a. Query a -> a -> HTTP.Query
    queryPrinter Raw q = q
    queryPrinter (Param key vIso) x = [(encodeUtf8 key, Just (encodeUtf8 (vIso.encode x)))]
    queryPrinter (Param' _ _) Nothing = []
    queryPrinter (Param' key vIso) (Just x) = [(encodeUtf8 key, Just (encodeUtf8 (vIso.encode x)))]
    queryPrinter (Param_ key vIso x) () = [(encodeUtf8 key, Just (encodeUtf8 (vIso.encode x)))]
    queryPrinter (Flag key) () = [(encodeUtf8 key, Nothing)]
    queryPrinter (Flag' key) True = [(encodeUtf8 key, Nothing)]
    queryPrinter (Flag' _) False = []
    queryPrinter (List style key vIso) nel = renderList style key vIso (NE.toList nel)
    queryPrinter (List' style key vIso) xs = renderList style key vIso xs

collectList :: ArrayStyle -> Text -> HTTP.Query -> ([ByteString], HTTP.Query)
collectList Exploded key q =
    let k = encodeUtf8 key
        (matched, rest) = partition (\(ik, _) -> ik == k) q
     in ([v | (_, Just v) <- matched], rest)
collectList style key q =
    let k = encodeUtf8 key
     in case partition (\(ik, _) -> ik == k) q of
            ([], _) -> ([], q)
            ((_, Nothing) : _, rest) -> ([], rest)
            ((_, Just v) : _, rest) -> (filter (not . BS.null) (BS8.split (delim style) v), rest)

renderList :: ArrayStyle -> Text -> Iso Query a -> [a] -> HTTP.Query
renderList Exploded key vIso xs = [(encodeUtf8 key, Just (enc x)) | x <- xs]
  where
    enc = encodeUtf8 . vIso.encode
renderList style key vIso xs
    | null xs = []
    | otherwise = [(encodeUtf8 key, Just (BS.intercalate (BS8.singleton (delim style)) (map enc xs)))]
  where
    enc = encodeUtf8 . vIso.encode

delim :: ArrayStyle -> Char
delim CommaDelimited = ','
delim SpaceDelimited = ' '
delim PipeDelimited = '|'
delim Exploded = ','

raw :: Codec Query HTTP.Query HTTP.Query
raw = Lift Raw

param :: Text -> Iso Query a -> Codec Query a a
param key vIso = Lift (Param key vIso)

param' :: Text -> Iso Query a -> Codec Query (Maybe a) (Maybe a)
param' key vIso = Lift (Param' key vIso)

param_ :: Text -> Iso Query a -> a -> Codec Query () ()
param_ key vIso x = Lift (Param_ key vIso x)

flag :: Text -> Codec Query () ()
flag key = Lift (Flag key)

flag' :: Text -> Codec Query Bool Bool
flag' key = Lift (Flag' key)

list :: ArrayStyle -> Text -> Iso Query a -> Codec Query (NonEmpty a) (NonEmpty a)
list style key vIso = Lift (List style key vIso)

list' :: ArrayStyle -> Text -> Iso Query a -> Codec Query [a] [a]
list' style key vIso = Lift (List' style key vIso)

instance Data Query Int        where iso = Iso (first (const ParseError) . parseQueryParam) toQueryParam (Info "integer" Nothing)
instance Data Query Int16      where iso = Iso (first (const ParseError) . parseQueryParam) toQueryParam (Info "integer" (Just "int32"))
instance Data Query Int32      where iso = Iso (first (const ParseError) . parseQueryParam) toQueryParam (Info "integer" (Just "int32"))
instance Data Query Int64      where iso = Iso (first (const ParseError) . parseQueryParam) toQueryParam (Info "integer" (Just "int64"))
instance Data Query Integer    where iso = Iso (first (const ParseError) . parseQueryParam) toQueryParam (Info "integer" Nothing)
instance Data Query Bool   where iso = Iso (first (const ParseError) . parseQueryParam) toQueryParam (Info "boolean" Nothing)
instance Data Query Float  where iso = Iso (first (const ParseError) . parseQueryParam) toQueryParam (Info "number" (Just "float"))
instance Data Query Double where iso = Iso (first (const ParseError) . parseQueryParam) toQueryParam (Info "number" (Just "double"))
instance Data Query Text   where iso = Iso (first (const ParseError) . parseQueryParam) toQueryParam (Info "string" Nothing)
instance Data Query UUID       where iso = Iso (first (const ParseError) . parseQueryParam) toQueryParam (Info "string" (Just "uuid"))
instance Data Query Day        where iso = Iso (first (const ParseError) . parseQueryParam) toQueryParam (Info "string" (Just "date"))
instance Data Query UTCTime    where iso = Iso (first (const ParseError) . parseQueryParam) toQueryParam (Info "string" (Just "date-time"))

class GQuery (f :: Type -> Type) where
    gQueryCodec :: Codec Query (f ()) (f ())

instance (GQuery f) => GQuery (D1 meta f) where
    gQueryCodec = FMap M1 $ LMap unM1 gQueryCodec

instance (GQuery f) => GQuery (C1 meta f) where
    gQueryCodec = FMap M1 $ LMap unM1 gQueryCodec

instance (GQuery f, GQuery g) => GQuery (f :*: g) where
    gQueryCodec =
        Apply
            (FMap (\l r -> l :*: r) (LMap (\(l :*: _) -> l) gQueryCodec))
            (LMap (\(_ :*: r) -> r) gQueryCodec)

instance (Selector s, Data Query a) => GQuery (S1 s (Rec0 a)) where
    gQueryCodec =
        let key = Text.pack (selName (undefined :: S1 s (Rec0 a) ()))
         in FMap (M1 . K1) $ LMap (unK1 . unM1) $ Lift (Param key (iso @Query @a))

instance {-# OVERLAPPING #-} (Selector s, Data Query a) => GQuery (S1 s (Rec0 (Maybe a))) where
    gQueryCodec =
        let key = Text.pack (selName (undefined :: S1 s (Rec0 (Maybe a)) ()))
         in FMap (M1 . K1) $ LMap (unK1 . unM1) $ Lift (Param' key (iso @Query @a))

instance {-# OVERLAPPING #-} (Selector s) => GQuery (S1 s (Rec0 ())) where
    gQueryCodec =
        let key = Text.pack (selName (undefined :: S1 s (Rec0 ()) ()))
         in FMap (M1 . K1) $ LMap (unK1 . unM1) $ Lift (Flag key)

instance {-# OVERLAPPING #-} (Selector s) => GQuery (S1 s (Rec0 Bool)) where
    gQueryCodec =
        let key = Text.pack (selName (undefined :: S1 s (Rec0 Bool) ()))
         in FMap (M1 . K1) $ LMap (unK1 . unM1) $ Lift (Flag' key)

queryCodec :: forall a. (Generic a, GQuery (Rep a)) => Codec Query a a
queryCodec = FMap (to @a) $ LMap (from @a) gQueryCodec
