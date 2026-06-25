{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Okapi.Protocol.Headers (
    Headers (..),
    ParseError (..),
    MediaType (..),
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
    mediaTypeBytes,
    contentType,
    cookie,
    cookie',
    setCookie,
    ConstF (..),
    GHeaders (..),
    headersCodec,
) where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.CaseInsensitive qualified as CI
import Data.Int (Int16, Int32, Int64)
import Data.Kind (Type)
import Data.List (find, partition)
import Data.Proxy (Proxy (..))
import Data.Scientific (Scientific)
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
import Prelude hiding (print)

import Okapi.Codec (Codec (..), ForRequest, ForResponse, ParseErrorOf, PartOf, StateOf)
import Okapi.Codec qualified as Codec
import Okapi.Data
    ( Data (..), Info (..), Iso (..), Prim (..)
    , boolPrim, charPrim, dayPrim, doublePrim, floatPrim, int16Prim, int32Prim
    , int64Prim, intPrim, integerPrim, localTimePrim, scientificPrim, textPrim
    , timeOfDayPrim, utcTimePrim, uuidPrim
    )
import Okapi.Protocol.Headers.Attributes (Attributes)
import Okapi.Protocol.Headers.Attributes qualified as Attr
import Okapi.Protocol.Headers.Cookies (Cookie)
import Okapi.Protocol.Headers.Structured (Structured)
import Okapi.Protocol.Headers.Structured qualified as Structured
import Okapi.Protocol.Headers.Structured.BareItem (BareItem)
import Okapi.Protocol.Headers.Structured.Item (Item, bareItem)
import Okapi.Protocol.Headers.Structured.List (List)
import Okapi.Protocol.Headers.Structured.Dictionary (Dictionary)



type Headers :: Type -> Type -> Type
data Headers ctx a where
    Raw             :: Headers ctx HTTP.RequestHeaders
    Field           :: HTTP.HeaderName -> Iso (Headers ctx) a -> Headers ctx a
    Field'          :: HTTP.HeaderName -> Iso (Headers ctx) a -> Headers ctx (Maybe a)
    Field_          :: HTTP.HeaderName -> ByteString -> Headers ctx ()
    FieldStructured :: HTTP.HeaderName -> Codec Structured a a -> Headers ctx a
    Cookie          :: ByteString -> Iso Cookie a -> Headers ForRequest a
    Cookie'         :: ByteString -> Iso Cookie a -> Headers ForRequest (Maybe a)
    SetCookie       :: ByteString -> Iso Cookie a -> Codec Attributes p p -> Headers ForResponse (a, p)

data ParseError = ParseError deriving (Eq, Show)

type instance StateOf      (Headers ctx) = [HTTP.Header]
type instance ParseErrorOf (Headers ctx) = ParseError
type instance PartOf       (Headers ctx) = ByteString



parse :: forall ctx i o. Codec (Headers ctx) i o -> [HTTP.Header] -> (Either ParseError o, [HTTP.Header])
parse = Codec.parser headersAlg
  where
    headersAlg :: forall a. Headers ctx a -> HTTP.RequestHeaders -> (Either ParseError a, HTTP.RequestHeaders)
    headersAlg Raw hs = (Right hs, [])
    headersAlg (Field key vIso) hs =
        case partition (\(k, _) -> k == key) hs of
            ([], _)          -> (Left ParseError, hs)
            ((_, v) : _, rest) -> case vIso.decode v of
                Left _  -> (Left ParseError, hs)
                Right x -> (Right x, rest)
    headersAlg (Field' key vIso) hs =
        case partition (\(k, _) -> k == key) hs of
            ([], _)          -> (Right Nothing, hs)
            ((_, v) : _, rest) -> case vIso.decode v of
                Left _  -> (Right Nothing, rest)
                Right x -> (Right (Just x), rest)
    headersAlg (Field_ k v) hs =
        case lookup k hs of
            Just v' | v' == v -> (Right (), filter (\(k', _) -> k' /= k) hs)
            _                 -> (Left ParseError, hs)
    headersAlg (FieldStructured name c) hs =
        case partition (\(k, _) -> k == name) hs of
            ([], _)          -> (Left ParseError, hs)
            ((_, v) : _, rest) ->
                case fst (Structured.parseStructured c v) of
                    Left _  -> (Left ParseError, hs)
                    Right x -> (Right x, rest)
    headersAlg (Cookie name vIso) hs =
        case lookup name (reqCookiePairs hs) of
            Just v  -> case vIso.decode v of
                Left _  -> (Left ParseError, hs)
                Right x -> (Right x, hs)
            Nothing -> (Left ParseError, hs)
    headersAlg (Cookie' name vIso) hs =
        case lookup name (reqCookiePairs hs) of
            Just v  -> (Right (either (const Nothing) Just (vIso.decode v)), hs)
            Nothing -> (Right Nothing, hs)
    headersAlg (SetCookie name vIso attrsC) hs =
        case find (\(n, _, _) -> n == name) (tokenizeSetCookies hs) of
            Just (_, v, attrs) -> case vIso.decode v of
                Left _  -> (Left ParseError, hs)
                Right x -> case fst (Attr.parse attrsC attrs) of
                    Left _  -> (Left ParseError, hs)
                    Right p -> (Right (x, p), hs)
            Nothing -> (Left ParseError, hs)

reqCookiePairs :: [HTTP.Header] -> WC.Cookies
reqCookiePairs hs = WC.parseCookies (BS.intercalate "; " [v | (n, v) <- hs, n == "cookie"])

tokenizeSetCookies :: [HTTP.Header] -> [(ByteString, ByteString, ByteString)]
tokenizeSetCookies hs = [tok v | (n, v) <- hs, n == "set-cookie"]
  where
    tok v =
        let (name, r1)   = BS.break (== 61) v
            (val, attrs) = BS.break (== 59) (BS.drop 1 r1)
         in (name, val, attrs)



print :: forall ctx i o. Codec (Headers ctx) i o -> i -> [HTTP.Header]
print c i = coalesceCookies (Codec.printer headersPrinter c i)
  where
    headersPrinter :: forall a. Headers ctx a -> a -> HTTP.RequestHeaders
    headersPrinter Raw hs = hs
    headersPrinter (Field key vIso) x = [(key, vIso.encode x)]
    headersPrinter (Field' _ _) Nothing = []
    headersPrinter (Field' key vIso) (Just x) = [(key, vIso.encode x)]
    headersPrinter (Field_ k v) () = [(k, v)]
    headersPrinter (FieldStructured name c) a = [(name, Structured.printStructured c a)]
    headersPrinter (Cookie name vIso) x = [("cookie", name <> "=" <> vIso.encode x)]
    headersPrinter (Cookie' _ _) Nothing = []
    headersPrinter (Cookie' name vIso) (Just x) = [("cookie", name <> "=" <> vIso.encode x)]
    headersPrinter (SetCookie name vIso attrsC) (x, p) =
        [("set-cookie", name <> "=" <> vIso.encode x <> Attr.print attrsC p)]

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



raw :: Codec (Headers ctx) HTTP.RequestHeaders HTTP.RequestHeaders
raw = Lift Raw

field :: HTTP.HeaderName -> Iso (Headers ctx) a -> Codec (Headers ctx) a a
field key vIso = Lift (Field key vIso)

field' :: HTTP.HeaderName -> Iso (Headers ctx) a -> Codec (Headers ctx) (Maybe a) (Maybe a)
field' key vIso = Lift (Field' key vIso)

field_ :: HTTP.HeaderName -> ByteString -> Codec (Headers ctx) h h -> Codec (Headers ctx) h h
field_ k v c = Apply (LMap (const ()) (FMap (const id) (Lift (Field_ k v)))) c

fieldStructured :: HTTP.HeaderName -> Codec Structured a a -> Codec (Headers ctx) a a
fieldStructured name c = Lift (FieldStructured name c)

fieldBareItem :: HTTP.HeaderName -> Iso BareItem a -> Codec (Headers ctx) a a
fieldBareItem name i = fieldStructured name (Structured.item (bareItem i))

fieldItem :: HTTP.HeaderName -> Codec Item a a -> Codec (Headers ctx) a a
fieldItem name c = fieldStructured name (Structured.item c)

fieldList :: HTTP.HeaderName -> Codec List a a -> Codec (Headers ctx) a a
fieldList name c = fieldStructured name (Structured.list c)

fieldDictionary :: HTTP.HeaderName -> Codec Dictionary a a -> Codec (Headers ctx) a a
fieldDictionary name c = fieldStructured name (Structured.dictionary c)

contentType :: MediaType -> Codec (Headers ctx) h h -> Codec (Headers ctx) h h
contentType mt = field_ "content-type" (mediaTypeBytes mt)

cookie :: ByteString -> Iso Cookie a -> Codec (Headers ForRequest) a a
cookie name vIso = Lift (Cookie name vIso)

cookie' :: ByteString -> Iso Cookie a -> Codec (Headers ForRequest) (Maybe a) (Maybe a)
cookie' name vIso = Lift (Cookie' name vIso)

setCookie :: ByteString -> Iso Cookie a -> Codec Attributes p p -> Codec (Headers ForResponse) (a, p) (a, p)
setCookie name vIso attrsC = Lift (SetCookie name vIso attrsC)



headerIso :: Prim a -> Iso (Headers ctx) a
headerIso (Prim dec enc nfo) =
    Iso (\bs -> either (const (Left ParseError)) Right (dec (decodeUtf8Lenient bs)))
        (encodeUtf8 . enc)
        nfo

instance Data (Headers ctx) Int        where iso = headerIso intPrim
instance Data (Headers ctx) Int16      where iso = headerIso int16Prim
instance Data (Headers ctx) Int32      where iso = headerIso int32Prim
instance Data (Headers ctx) Int64      where iso = headerIso int64Prim
instance Data (Headers ctx) Integer    where iso = headerIso integerPrim
instance Data (Headers ctx) Float      where iso = headerIso floatPrim
instance Data (Headers ctx) Double     where iso = headerIso doublePrim
instance Data (Headers ctx) Scientific where iso = headerIso scientificPrim
instance Data (Headers ctx) Bool       where iso = headerIso boolPrim
instance Data (Headers ctx) Char       where iso = headerIso charPrim
instance Data (Headers ctx) Text.Text  where iso = headerIso textPrim
instance Data (Headers ctx) Day        where iso = headerIso dayPrim
instance Data (Headers ctx) LocalTime  where iso = headerIso localTimePrim
instance Data (Headers ctx) UTCTime    where iso = headerIso utcTimePrim
instance Data (Headers ctx) TimeOfDay  where iso = headerIso timeOfDayPrim
instance Data (Headers ctx) UUID       where iso = headerIso uuidPrim

instance Data (Headers ctx) ByteString where
    iso = Iso Right id (Info "string" Nothing)

instance Data (Headers ctx) DiffTime where
    iso = headerIso (Prim dec enc (Info "number" Nothing))
      where
        enc = Text.pack . show . (realToFrac :: DiffTime -> Double)
        dec t = case (readMaybe (Text.unpack t) :: Maybe Double) of
            Just d  -> Right (realToFrac d)
            Nothing -> Left ("invalid difftime: " <> t)

instance Data (Headers ctx) (TimeOfDay, TimeZone) where
    iso = headerIso (Prim dec enc (Info "string" (Just "time")))
      where
        enc (tod, tz) = Text.pack (formatTime defaultTimeLocale "%T" tod <> timeZoneOffsetString tz)
        dec t = case parseTimeM True defaultTimeLocale "%T%z" (Text.unpack t) of
            Just zt -> Right (localTimeOfDay (zonedTimeToLocalTime zt), zonedTimeZone zt)
            Nothing -> Left ("invalid (TimeOfDay, TimeZone): " <> t)



data ConstF (val :: Symbol) = ConstF deriving (Eq, Show)

fieldToHeaderName :: String -> HTTP.HeaderName
fieldToHeaderName = CI.mk . encodeUtf8 . Text.pack . map (\c -> if c == '_' then '-' else c)

class GHeaders ctx (f :: Type -> Type) where
    gHeadersCodec :: Codec (Headers ctx) (f ()) (f ())

instance (GHeaders ctx f) => GHeaders ctx (D1 meta f) where
    gHeadersCodec = FMap M1 $ LMap unM1 gHeadersCodec

instance (GHeaders ctx f) => GHeaders ctx (C1 meta f) where
    gHeadersCodec = FMap M1 $ LMap unM1 gHeadersCodec

instance (GHeaders ctx f, GHeaders ctx g) => GHeaders ctx (f :*: g) where
    gHeadersCodec =
        Apply
            (FMap (\l r -> l :*: r) (LMap (\(l :*: _) -> l) gHeadersCodec))
            (LMap (\(_ :*: r) -> r) gHeadersCodec)

instance (Selector s, Data (Headers ctx) a) => GHeaders ctx (S1 s (Rec0 a)) where
    gHeadersCodec =
        let key = fieldToHeaderName (selName (undefined :: S1 s (Rec0 a) ()))
         in FMap (M1 . K1) $ LMap (unK1 . unM1) $ Lift (Field key (iso @(Headers ctx) @a))

instance {-# OVERLAPPING #-} (Selector s, Data (Headers ctx) a) => GHeaders ctx (S1 s (Rec0 (Maybe a))) where
    gHeadersCodec =
        let key = fieldToHeaderName (selName (undefined :: S1 s (Rec0 (Maybe a)) ()))
         in FMap (M1 . K1) $ LMap (unK1 . unM1) $ Lift (Field' key (iso @(Headers ctx) @a))

instance {-# OVERLAPPING #-} (Selector s, KnownSymbol val) => GHeaders ctx (S1 s (Rec0 (ConstF val))) where
    gHeadersCodec =
        let k = fieldToHeaderName (selName (undefined :: S1 s (Rec0 (ConstF val)) ()))
            v = encodeUtf8 (Text.pack (symbolVal (Proxy @val)))
         in FMap (\() -> M1 (K1 ConstF)) $ LMap (const ()) $ Lift (Field_ k v)

headersCodec :: forall ctx a. (Generic a, GHeaders ctx (Rep a)) => Codec (Headers ctx) a a
headersCodec = FMap (to @a) $ LMap (from @a) (gHeadersCodec @ctx @(Rep a))

