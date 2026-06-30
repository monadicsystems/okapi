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

module Okapi.HTTP.Request.Headers (
    Headers (..),
    ParseError (..),
    parse,
    print,
    raw,
    field,
    field',
    field_,
    fieldStructured,
    fieldBareItem,
    fieldItem,
    fieldList,
    fieldDictionary,
    contentType,
    cookie,
    cookie',
    MediaType (..),
    ConstF (..),
    GHeaders (..),
    headersCodec,
) where

import Data.Bifunctor (first)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.CaseInsensitive qualified as CI
import Data.Int (Int16, Int32, Int64)
import Data.Kind (Type)
import Data.List (partition)
import Data.Proxy (Proxy (..))
import Data.Text qualified as Text
import Data.Text.Encoding (decodeUtf8Lenient, encodeUtf8)
import Data.Time (Day, DiffTime, LocalTime, TimeOfDay, TimeZone, UTCTime, localTimeOfDay, timeZoneOffsetString, zonedTimeToLocalTime, zonedTimeZone)
import Data.Time.Format (defaultTimeLocale, formatTime, parseTimeM)
import Data.UUID (UUID)
import GHC.Generics (C1, D1, Generic (..), K1 (..), M1 (..), Rec0, S1, Selector (..), (:*:) (..))
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)
import Network.HTTP.Types qualified as HTTP
import Text.Read (readMaybe)
import Web.Cookie qualified as WC
import Web.HttpApiData (parseHeader, toHeader)
import Prelude hiding (print)
import Okapi.Leaf (ErrorOf, HasLeaf (..), Info (..), Leaf (..), PieceOf, StateOf)
import Okapi.Tree (Tree (..))
import Okapi.Tree qualified as Tree
import Okapi.HTTP.Headers.Cookies (Cookie)
import Okapi.HTTP.Headers.Structured (Structured)
import Okapi.HTTP.Headers.Structured qualified as Structured
import Okapi.HTTP.Headers.Structured.BareItem (BareItem)
import Okapi.HTTP.Headers.Structured.Item (Item, bareItem)
import Okapi.HTTP.Headers.Structured.List (List)
import Okapi.HTTP.Headers.Structured.Dictionary (Dictionary)

type Headers :: Type -> Type
data Headers a where
    Raw             :: Headers HTTP.RequestHeaders
    Field           :: HTTP.HeaderName -> Leaf Headers a -> Headers a
    Field'          :: HTTP.HeaderName -> Leaf Headers a -> Headers (Maybe a)
    Field_          :: HTTP.HeaderName -> ByteString -> Headers ()
    FieldStructured :: HTTP.HeaderName -> Tree Structured a a -> Headers a
    Cookie          :: ByteString -> Leaf Cookie a -> Headers a
    Cookie'         :: ByteString -> Leaf Cookie a -> Headers (Maybe a)

data ParseError = ParseError deriving (Eq, Show)

type instance StateOf Headers = [HTTP.Header]
type instance ErrorOf Headers = ParseError
type instance PieceOf Headers = ByteString

parse :: Tree Headers i o -> [HTTP.Header] -> (Either ParseError o, [HTTP.Header])
parse = Tree.grow headersAlg
  where
    headersAlg :: forall a. Headers a -> HTTP.RequestHeaders -> (Either ParseError a, HTTP.RequestHeaders)
    headersAlg Raw hs = (Right hs, [])
    headersAlg (Field key vLeaf) hs =
        case partition (\(k, _) -> k == key) hs of
            ([], _)            -> (Left ParseError, hs)
            ((_, v) : _, rest) -> case vLeaf.decode v of
                Left _  -> (Left ParseError, hs)
                Right x -> (Right x, rest)
    headersAlg (Field' key vLeaf) hs =
        case partition (\(k, _) -> k == key) hs of
            ([], _)            -> (Right Nothing, hs)
            ((_, v) : _, rest) -> case vLeaf.decode v of
                Left _  -> (Right Nothing, rest)
                Right x -> (Right (Just x), rest)
    headersAlg (Field_ k v) hs =
        case lookup k hs of
            Just v' | v' == v -> (Right (), filter (\(k', _) -> k' /= k) hs)
            _                 -> (Left ParseError, hs)
    headersAlg (FieldStructured name c) hs =
        case partition (\(k, _) -> k == name) hs of
            ([], _)            -> (Left ParseError, hs)
            ((_, v) : _, rest) -> case fst (Structured.parseStructured c v) of
                Left _  -> (Left ParseError, hs)
                Right x -> (Right x, rest)
    headersAlg (Cookie name vLeaf) hs =
        case lookup name (reqCookiePairs hs) of
            Just v  -> case vLeaf.decode v of
                Left _  -> (Left ParseError, hs)
                Right x -> (Right x, hs)
            Nothing -> (Left ParseError, hs)
    headersAlg (Cookie' name vLeaf) hs =
        case lookup name (reqCookiePairs hs) of
            Just v  -> (Right (either (const Nothing) Just (vLeaf.decode v)), hs)
            Nothing -> (Right Nothing, hs)

reqCookiePairs :: [HTTP.Header] -> WC.Cookies
reqCookiePairs hs = WC.parseCookies (BS.intercalate "; " [v | (n, v) <- hs, n == "cookie"])

print :: Tree Headers i o -> i -> [HTTP.Header]
print c i = coalesceCookies (Tree.eat headersPrinter c i)
  where
    headersPrinter :: forall a. Headers a -> a -> HTTP.RequestHeaders
    headersPrinter Raw hs = hs
    headersPrinter (Field key vLeaf) x = [(key, vLeaf.encode x)]
    headersPrinter (Field' _ _) Nothing = []
    headersPrinter (Field' key vLeaf) (Just x) = [(key, vLeaf.encode x)]
    headersPrinter (Field_ k v) () = [(k, v)]
    headersPrinter (FieldStructured name c) a = [(name, Structured.printStructured c a)]
    headersPrinter (Cookie name vLeaf) x = [("cookie", name <> "=" <> vLeaf.encode x)]
    headersPrinter (Cookie' _ _) Nothing = []
    headersPrinter (Cookie' name vLeaf) (Just x) = [("cookie", name <> "=" <> vLeaf.encode x)]

coalesceCookies :: [HTTP.Header] -> [HTTP.Header]
coalesceCookies hs =
    let (cks, rest) = partition ((== "cookie") . fst) hs
     in rest ++ [("cookie", BS.intercalate "; " (map snd cks)) | not (null cks)]

data MediaType
    = JSON
    | HTML
    | PlainText
    | FormUrlEncoded
    | OctetStream
    | EventStream
    | Custom ByteString
    deriving (Eq, Show)

mediaTypeBytes :: MediaType -> ByteString
mediaTypeBytes JSON           = "application/json"
mediaTypeBytes HTML           = "text/html"
mediaTypeBytes PlainText      = "text/plain"
mediaTypeBytes FormUrlEncoded = "application/x-www-form-urlencoded"
mediaTypeBytes OctetStream    = "application/octet-stream"
mediaTypeBytes EventStream    = "text/event-stream"
mediaTypeBytes (Custom bs)    = bs

raw :: Tree Headers HTTP.RequestHeaders HTTP.RequestHeaders
raw = Node Raw

field :: HTTP.HeaderName -> Leaf Headers a -> Tree Headers a a
field key vLeaf = Node (Field key vLeaf)

field' :: HTTP.HeaderName -> Leaf Headers a -> Tree Headers (Maybe a) (Maybe a)
field' key vLeaf = Node (Field' key vLeaf)

field_ :: HTTP.HeaderName -> ByteString -> Tree Headers h h -> Tree Headers h h
field_ k v c = Apply (LMap (const ()) (FMap (const id) (Node (Field_ k v)))) c

fieldStructured :: HTTP.HeaderName -> Tree Structured a a -> Tree Headers a a
fieldStructured name c = Node (FieldStructured name c)

fieldBareItem :: HTTP.HeaderName -> Leaf BareItem a -> Tree Headers a a
fieldBareItem name i = fieldStructured name (Structured.item (bareItem i))

fieldItem :: HTTP.HeaderName -> Tree Item a a -> Tree Headers a a
fieldItem name c = fieldStructured name (Structured.item c)

fieldList :: HTTP.HeaderName -> Tree List a a -> Tree Headers a a
fieldList name c = fieldStructured name (Structured.list c)

fieldDictionary :: HTTP.HeaderName -> Tree Dictionary a a -> Tree Headers a a
fieldDictionary name c = fieldStructured name (Structured.dictionary c)

contentType :: MediaType -> Tree Headers h h -> Tree Headers h h
contentType mt = field_ "content-type" (mediaTypeBytes mt)

cookie :: ByteString -> Leaf Cookie a -> Tree Headers a a
cookie name vLeaf = Node (Cookie name vLeaf)

cookie' :: ByteString -> Leaf Cookie a -> Tree Headers (Maybe a) (Maybe a)
cookie' name vLeaf = Node (Cookie' name vLeaf)

instance HasLeaf Headers Int       where leaf = Leaf (first (const ParseError) . parseHeader) toHeader (Info "integer" Nothing)
instance HasLeaf Headers Int16     where leaf = Leaf (first (const ParseError) . parseHeader) toHeader (Info "integer" (Just "int32"))
instance HasLeaf Headers Int32     where leaf = Leaf (first (const ParseError) . parseHeader) toHeader (Info "integer" (Just "int32"))
instance HasLeaf Headers Int64     where leaf = Leaf (first (const ParseError) . parseHeader) toHeader (Info "integer" (Just "int64"))
instance HasLeaf Headers Integer   where leaf = Leaf (first (const ParseError) . parseHeader) toHeader (Info "integer" Nothing)
instance HasLeaf Headers Bool      where leaf = Leaf (first (const ParseError) . parseHeader) toHeader (Info "boolean" Nothing)
instance HasLeaf Headers Float     where leaf = Leaf (first (const ParseError) . parseHeader) toHeader (Info "number" (Just "float"))
instance HasLeaf Headers Double    where leaf = Leaf (first (const ParseError) . parseHeader) toHeader (Info "number" (Just "double"))
instance HasLeaf Headers Text.Text where leaf = Leaf (first (const ParseError) . parseHeader) toHeader (Info "string" Nothing)
instance HasLeaf Headers UUID      where leaf = Leaf (first (const ParseError) . parseHeader) toHeader (Info "string" (Just "uuid"))
instance HasLeaf Headers Day       where leaf = Leaf (first (const ParseError) . parseHeader) toHeader (Info "string" (Just "date"))
instance HasLeaf Headers LocalTime where leaf = Leaf (first (const ParseError) . parseHeader) toHeader (Info "string" (Just "date-time"))
instance HasLeaf Headers UTCTime   where leaf = Leaf (first (const ParseError) . parseHeader) toHeader (Info "string" (Just "date-time"))
instance HasLeaf Headers TimeOfDay where leaf = Leaf (first (const ParseError) . parseHeader) toHeader (Info "string" (Just "time"))

instance HasLeaf Headers ByteString where
    leaf = Leaf Right id (Info "string" Nothing)

instance HasLeaf Headers DiffTime where
    leaf = Leaf dec (encodeUtf8 . Text.pack . show . (realToFrac :: DiffTime -> Double)) (Info "number" Nothing)
      where
        dec bs = case (readMaybe (Text.unpack (decodeUtf8Lenient bs)) :: Maybe Double) of
            Just d  -> Right (realToFrac d)
            Nothing -> Left ParseError

instance HasLeaf Headers (TimeOfDay, TimeZone) where
    leaf = Leaf dec enc (Info "string" (Just "time"))
      where
        enc (tod, tz) = encodeUtf8 (Text.pack (formatTime defaultTimeLocale "%T" tod <> timeZoneOffsetString tz))
        dec bs = case parseTimeM True defaultTimeLocale "%T%z" (Text.unpack (decodeUtf8Lenient bs)) of
            Just zt -> Right (localTimeOfDay (zonedTimeToLocalTime zt), zonedTimeZone zt)
            Nothing -> Left ParseError

data ConstF (val :: Symbol) = ConstF deriving (Eq, Show)

fieldToHeaderName :: String -> HTTP.HeaderName
fieldToHeaderName = CI.mk . encodeUtf8 . Text.pack . map (\c -> if c == '_' then '-' else c)

class GHeaders (f :: Type -> Type) where
    gHeadersCodec :: Tree Headers (f ()) (f ())

instance (GHeaders f) => GHeaders (D1 meta f) where
    gHeadersCodec = FMap M1 $ LMap unM1 gHeadersCodec

instance (GHeaders f) => GHeaders (C1 meta f) where
    gHeadersCodec = FMap M1 $ LMap unM1 gHeadersCodec

instance (GHeaders f, GHeaders g) => GHeaders (f :*: g) where
    gHeadersCodec =
        Apply
            (FMap (\l r -> l :*: r) (LMap (\(l :*: _) -> l) gHeadersCodec))
            (LMap (\(_ :*: r) -> r) gHeadersCodec)

instance (Selector s, HasLeaf Headers a) => GHeaders (S1 s (Rec0 a)) where
    gHeadersCodec =
        let key = fieldToHeaderName (selName (undefined :: S1 s (Rec0 a) ()))
         in FMap (M1 . K1) $ LMap (unK1 . unM1) $ Node (Field key (leaf @Headers @a))

instance {-# OVERLAPPING #-} (Selector s, HasLeaf Headers a) => GHeaders (S1 s (Rec0 (Maybe a))) where
    gHeadersCodec =
        let key = fieldToHeaderName (selName (undefined :: S1 s (Rec0 (Maybe a)) ()))
         in FMap (M1 . K1) $ LMap (unK1 . unM1) $ Node (Field' key (leaf @Headers @a))

instance {-# OVERLAPPING #-} (Selector s, KnownSymbol val) => GHeaders (S1 s (Rec0 (ConstF val))) where
    gHeadersCodec =
        let k = fieldToHeaderName (selName (undefined :: S1 s (Rec0 (ConstF val)) ()))
            v = encodeUtf8 (Text.pack (symbolVal (Proxy @val)))
         in FMap (\() -> M1 (K1 ConstF)) $ LMap (const ()) $ Node (Field_ k v)

headersCodec :: forall a. (Generic a, GHeaders (Rep a)) => Tree Headers a a
headersCodec = FMap (to @a) $ LMap (from @a) gHeadersCodec
