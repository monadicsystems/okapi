{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

-- | The header codec shared by request and response headers alike. 'field'
--   and friends are free in the phantom @ctx@ and work unqualified for
--   either side; only 'cookie'\/'cookie'' (request-only), 'setCookie'
--   (response-only), and 'coalesceCookies' (request-only, see its own
--   haddock) are pinned to a specific side.
module Okapi.HTTP.Headers (
    Headers (..),
    Base,
    ParseError (..),
    parser,
    printer,
    parseExact,
    base,
    field,
    field',
    field_,
    fieldStruct,
    fieldBareItem,
    fieldItem,
    fieldList,
    fieldDict,
    contentType,
    cookie,
    cookie',
    coalesceCookies,
    setCookie,
    MediaType (..),
    mediaTypeBytes,
    ConstF (..),
    fieldToHeaderName,
    GHeaders (..),
    derived,
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
import Network.HTTP.Types qualified as Types
import Text.Read (readMaybe)
import Web.Cookie qualified as WC
import Web.HttpApiData (parseHeader, toHeader)
import Okapi.HTTP.Tree (ForRequest, ForResponse, Failure, HasLeaf (..), Info (..), Leaf (..), Parser, Printer, Piece, Context, Tree (..))
import Okapi.HTTP.Tree qualified as Tree
import Okapi.HTTP.Request.Headers.Cookie (Cookie)
import Okapi.HTTP.Response.Headers.SetCookie (SetCookie)
import Okapi.HTTP.Response.Headers.Attributes (Attributes)
import Okapi.HTTP.Response.Headers.Attributes qualified as Attributes
import Okapi.HTTP.Structured (Structured)
import Okapi.HTTP.Structured qualified as Structured
import Okapi.HTTP.Structured.BareItem (BareItem)
import Okapi.HTTP.Structured.Item (Item, bareItem)
import Okapi.HTTP.Structured.List (List)
import Okapi.HTTP.Structured.Dictionary (Dictionary)

-- $setup
-- >>> :set -XApplicativeDo
-- >>> import Okapi.HTTP.Tree (Leaf, printParse, int, integer, (=.))
-- >>> import Okapi.HTTP.Structured.BareItem (BareItem)
-- >>> import Okapi.HTTP.Structured.Dictionary qualified as Dictionary
-- >>> import Okapi.HTTP.Structured.Item qualified as Item
-- >>> import GHC.Generics (Generic)
-- >>> :{
-- let twoFields = do
--       a <- fst =. field "x-a" int
--       b <- snd =. field "x-b" int
--       pure (a, b)
--     twoCookies = do
--       a <- fst =. cookie "sid" int
--       b <- snd =. cookie "tok" int
--       pure (a, b)
--     versioned = do
--       contentType JSON
--       field_ "x-api-key" "secret"
--       x <- field "x-id" int
--       pure x
-- :}
--
-- >>> :{
-- data Page = Page { x_a :: Int, x_b :: Int } deriving (Generic, Eq, Show)
-- :}

type Headers :: Type -> Type -> Type -> Type
data Headers ctx i o where
    Base             :: Headers ctx Base Base
    Field           :: Types.HeaderName -> Leaf (Headers ctx) a -> Headers ctx a a
    Field'          :: Types.HeaderName -> Leaf (Headers ctx) a -> Headers ctx (Maybe a) (Maybe a)
    Field_          :: Types.HeaderName -> ByteString -> Headers ctx i ()
    FieldStructured :: Types.HeaderName -> Tree Structured a a -> Headers ctx a a
    Cookie          :: ByteString -> Leaf Cookie a -> Headers ForRequest a a
    Cookie'         :: ByteString -> Leaf Cookie a -> Headers ForRequest (Maybe a) (Maybe a)
    SetCookie       :: ByteString -> Leaf SetCookie a -> Tree Attributes p p -> Headers ForResponse (a, p) (a, p)

data ParseError = ParseError deriving (Eq, Show)

type instance Context (Headers ctx) = [Types.Header]
type instance Failure (Headers ctx) = ParseError
type instance Piece (Headers ctx) = ByteString

reqCookiePairs :: [Types.Header] -> WC.Cookies
reqCookiePairs hs = WC.parseCookies (BS.intercalate "; " [v | (n, v) <- hs, n == "cookie"])

parser :: Tree (Headers ctx) i o -> Parser (Headers ctx) o
parser = Tree.parser alg
  where
    alg :: Headers ctx i o -> Parser (Headers ctx) o
    alg Base hs = (Right hs, [])
    alg (Field key vLeaf) hs =
        case partition (\(k, _) -> k == key) hs of
            ([], _)            -> (Left ParseError, hs)
            ((_, v) : _, rest) -> case vLeaf.decode v of
                Left _  -> (Left ParseError, hs)
                Right x -> (Right x, rest)
    alg (Field' key vLeaf) hs =
        case partition (\(k, _) -> k == key) hs of
            ([], _)            -> (Right Nothing, hs)
            ((_, v) : _, rest) -> case vLeaf.decode v of
                Left _  -> (Right Nothing, rest)
                Right x -> (Right (Just x), rest)
    alg (Field_ k v) hs =
        case lookup k hs of
            Just v' | v' == v -> (Right (), filter (\(k', _) -> k' /= k) hs)
            _                 -> (Left ParseError, hs)
    alg (FieldStructured name c) hs =
        case partition (\(k, _) -> k == name) hs of
            ([], _)            -> (Left ParseError, hs)
            ((_, v) : _, rest) -> case Structured.parser c v of
                (Left _, _)              -> (Left ParseError, hs)
                (Right x, leftover)
                    | BS.null leftover  -> (Right x, rest)
                    | otherwise         -> (Right x, (name, leftover) : rest)
    alg (Cookie name vLeaf) hs =
        let cookies = reqCookiePairs hs
            (_, nonCookieHs) = partition ((== "cookie") . fst) hs
            remaining = filter ((/= name) . fst) cookies
            newHs = nonCookieHs ++ [("cookie", BS.intercalate "; " [k <> "=" <> v | (k, v) <- remaining]) | not (null remaining)]
        in case lookup name cookies of
            Nothing -> (Left ParseError, hs)
            Just v  -> case vLeaf.decode v of
                Left _  -> (Left ParseError, hs)
                Right x -> (Right x, newHs)
    alg (Cookie' name vLeaf) hs =
        let cookies = reqCookiePairs hs
            (_, nonCookieHs) = partition ((== "cookie") . fst) hs
            remaining = filter ((/= name) . fst) cookies
            newHs = nonCookieHs ++ [("cookie", BS.intercalate "; " [k <> "=" <> v | (k, v) <- remaining]) | not (null remaining)]
        in case lookup name cookies of
            Nothing -> (Right Nothing, hs)
            Just v  -> (Right (either (const Nothing) Just (vLeaf.decode v)), newHs)
    alg (SetCookie cookieName vLeaf attrsC) hs =
        let tok v = let (n, r1) = BS.break (== 61) v
                        (val, attrs) = BS.break (== 59) (BS.drop 1 r1)
                    in (n, val, attrs)
            (matched, rest) = partition (\(k, v) -> k == "set-cookie" && (\(n, _, _) -> n == cookieName) (tok v)) hs
        in case matched of
            []         -> (Left ParseError, hs)
            (_, v) : _ ->
                let (_, val, attrs) = tok v
                in case vLeaf.decode val of
                    Left _  -> (Left ParseError, hs)
                    Right x -> case fst (Attributes.parser attrsC attrs) of
                        Left _  -> (Left ParseError, hs)
                        Right p -> (Right (x, p), rest)

printer :: Tree (Headers ctx) i o -> Printer (Headers ctx) i
printer = Tree.printer alg
  where
    alg :: Headers ctx i o -> Printer (Headers ctx) i
    alg Base                           hs       = hs
    alg (Field key vLeaf)             x        = [(key, vLeaf.encode x)]
    alg (Field' _ _)                  Nothing  = []
    alg (Field' key vLeaf)            (Just x) = [(key, vLeaf.encode x)]
    alg (Field_ k v)                  _        = [(k, v)]
    alg (FieldStructured name c)      a        = [(name, Structured.printer c a)]
    alg (Cookie name vLeaf)           x        = [("cookie", name <> "=" <> vLeaf.encode x)]
    alg (Cookie' _ _)                 Nothing  = []
    alg (Cookie' name vLeaf)          (Just x) = [("cookie", name <> "=" <> vLeaf.encode x)]
    alg (SetCookie name vLeaf attrsC) (x, p)   =
        [("set-cookie", name <> "=" <> vLeaf.encode x <> Attributes.printer attrsC p)]

-- | Require full consumption — 'Left' with the leftover headers (which may
--   include partially-consumed RFC 9651 values still under their original
--   header name, see 'fieldStruct') if any remain, 'Left' with the
--   underlying error if parsing itself failed.
parseExact :: Tree (Headers ctx) i o -> [Types.Header] -> Either (Either ParseError [Types.Header]) o
parseExact = Tree.parseExact parser

-- | Pass all headers straight through, unconstrained.
--
-- >>> parser base [("x-a", "1"), ("x-b", "2")]
-- (Right [("x-a","1"),("x-b","2")],[])
-- >>> printer base [("x-a", "1"), ("x-b", "2")]
-- [("x-a","1"),("x-b","2")]
base :: Tree (Headers ctx) [Types.Header] [Types.Header]
base = Node Base

-- | What 'base' decodes\/encodes to — the maximally unconstrained headers
--   slot, shared by both sides (@'Headers' 'Okapi.HTTP.Tree.ForRequest'@
--   and @'Headers' 'Okapi.HTTP.Tree.ForResponse'@ are both just
--   @['Types.Header']@ underneath).
type Base = [Types.Header]

-- | Parse and print a required header field.
--
-- prop> printParse parser printer (field "x-foo" int) (x :: Int)
field :: Types.HeaderName -> Leaf (Headers ctx) a -> Tree (Headers ctx) a a
field key vLeaf = Node (Field key vLeaf)

-- | Parse and print an optional header field.
--
-- prop> printParse parser printer (field' "x-foo" int) (x :: Maybe Int)
field' :: Types.HeaderName -> Leaf (Headers ctx) a -> Tree (Headers ctx) (Maybe a) (Maybe a)
field' key vLeaf = Node (Field' key vLeaf)

-- | A required header field matched against a fixed literal value, e.g.
--   the assertion in the RFC 9651 §3.2 boolean-shorthand style — matches
--   'Okapi.HTTP.Request.Path.seg_'\'s shape: contributes @()@ and
--   accepts any input, so it composes with siblings of /any/ type via
--   ordinary 'Applicative'\/@do@-block sequencing, no explicit alignment
--   needed — not a wrapping combinator that takes "the rest of your
--   headers" as an explicit argument.
--
-- >>> parser (field_ "x-version" "2") [("x-version", "2")]
-- (Right (),[])
-- >>> parser (field_ "x-version" "2") [("x-version", "1")]
-- (Left ParseError,[("x-version","1")])
-- >>> printer (field_ "x-version" "2") ()
-- [("x-version","2")]
--
-- Composes directly with value-producing siblings in the same @do@\/
-- 'Applicative' block — no explicit alignment needed, since its own input
-- is unconstrained (the shape every real request\/response, e.g. asserting
-- a fixed API version while also reading a real field, actually needs —
-- see 'versioned' in "$setup"):
--
-- >>> parser versioned [("content-type", "application/json"), ("x-api-key", "secret"), ("x-id", "5")]
-- (Right 5,[])
-- >>> printer versioned 5
-- [("content-type","application/json"),("x-api-key","secret"),("x-id","5")]
--
-- prop> printParse parser printer versioned (x :: Int)
field_ :: Types.HeaderName -> ByteString -> Tree (Headers ctx) i ()
field_ k v = Node (Field_ k v)

-- | A header field whose value is parsed\/printed via an RFC 9651 codec.
--   If @c@ doesn't consume the entire header value, the unrecognized
--   remainder is left in the context under the /same/ header name — same
--   discipline as 'field'\/'seg' leaving whatever they don't consume
--   for later combinators, just at the granularity of one header's value
--   instead of the whole context. A later combinator sharing that name
--   can continue parsing from exactly where this one left off (see
--   'fieldDict' for the motivating use case); use 'parseExact' if
--   you need to assert nothing was left over anywhere.
fieldStruct :: Types.HeaderName -> Tree Structured a a -> Tree (Headers ctx) a a
fieldStruct name c = Node (FieldStructured name c)

-- | A header field whose value is parsed\/printed as an RFC 9651
--   Structured Field Value Item wrapping a single bare value. Trailing
--   content in the header value that @i@ doesn't recognize is left under
--   the same header name (see 'fieldStruct') rather than erroring —
--   'parseExact' catches it if that's not wanted:
--
-- >>> parser (fieldBareItem "x-count" (integer :: Leaf BareItem Integer)) [("x-count", "5")]
-- (Right 5,[])
-- >>> parser (fieldBareItem "x-count" (integer :: Leaf BareItem Integer)) [("x-count", "5;bogus")]
-- (Right 5,[("x-count",";bogus")])
-- >>> parseExact (fieldBareItem "x-count" (integer :: Leaf BareItem Integer)) [("x-count", "5;bogus")]
-- Left (Right [("x-count",";bogus")])
-- >>> printer (fieldBareItem "x-count" (integer :: Leaf BareItem Integer)) 5
-- [("x-count","5")]
fieldBareItem :: Types.HeaderName -> Leaf BareItem a -> Tree (Headers ctx) a a
fieldBareItem name i = fieldStruct name (Structured.item (bareItem i))

fieldItem :: Types.HeaderName -> Tree Item a a -> Tree (Headers ctx) a a
fieldItem name c = fieldStruct name (Structured.item c)

fieldList :: Types.HeaderName -> Tree List a a -> Tree (Headers ctx) a a
fieldList name c = fieldStruct name (Structured.list c)

-- | A header field whose value is parsed\/printed as an RFC 9651
--   Dictionary. Since 'fieldStruct' only consumes what @c@ recognizes,
--   two 'fieldDict's sharing the same header name — each matching a
--   different member — correctly split a single multi-member value across
--   two differently-typed record fields, combined via 'Okapi.HTTP.Tree.Apply':
--
-- >>> :{
-- let two = (,) <$> (fst =. fieldDict "x" (Dictionary.member "a" (Item.bareItem (integer :: Leaf BareItem Integer))))
--               <*> (snd =. fieldDict "x" (Dictionary.member "b" (Item.bareItem (integer :: Leaf BareItem Integer))))
-- :}
--
-- >>> parser two [("x", "a=1, b=2")]
-- (Right (1,2),[])
fieldDict :: Types.HeaderName -> Tree Dictionary a a -> Tree (Headers ctx) a a
fieldDict name c = fieldStruct name (Structured.dict c)

-- | Asserts the @content-type@ header matches exactly — same shape as
--   'field_', see there.
--
-- >>> parser (contentType JSON) [("content-type", "application/json")]
-- (Right (),[])
contentType :: MediaType -> Tree (Headers ctx) i ()
contentType mt = field_ "content-type" (mediaTypeBytes mt)

-- | Parse and print a required cookie.
--
-- prop> printParse parser printer (cookie "sid" int) (x :: Int)
cookie :: ByteString -> Leaf Cookie a -> Tree (Headers ForRequest) a a
cookie name vLeaf = Node (Cookie name vLeaf)

-- | Parse and print an optional cookie.
--
-- prop> printParse parser printer (cookie' "sid" int) (x :: Maybe Int)
cookie' :: ByteString -> Leaf Cookie a -> Tree (Headers ForRequest) (Maybe a) (Maybe a)
cookie' name vLeaf = Node (Cookie' name vLeaf)

-- | Merge multiple @cookie:@ headers into one, per RFC 6265 §5.4 —
--   request-only (there's no equivalent merge rule for @Set-Cookie@,
--   where multiple headers are the normal way to set multiple cookies).
--   Printing needs this explicitly since the generic 'printer' above
--   builds header lists one field at a time — applied once at the end by
--   "Okapi.HTTP.Request"'s own @printer@, not inside 'printer' here.
coalesceCookies :: [Types.Header] -> [Types.Header]
coalesceCookies hs =
    let (cks, rest) = partition ((== "cookie") . fst) hs
     in rest ++ [("cookie", BS.intercalate "; " (map snd cks)) | not (null cks)]

-- | Parse and print a @Set-Cookie@ header — the cookie's value via @vLeaf@,
--   its attribute list (@Max-Age@, @Domain@, @Secure@, ...) via @attrsC@.
setCookie :: ByteString -> Leaf SetCookie a -> Tree Attributes p p -> Tree (Headers ForResponse) (a, p) (a, p)
setCookie name vLeaf attrsC = Node (SetCookie name vLeaf attrsC)

instance HasLeaf (Headers ctx) Int       where leaf = Leaf (first (const ParseError) . parseHeader) toHeader (Info "integer" Nothing)
instance HasLeaf (Headers ctx) Int16     where leaf = Leaf (first (const ParseError) . parseHeader) toHeader (Info "integer" (Just "int32"))
instance HasLeaf (Headers ctx) Int32     where leaf = Leaf (first (const ParseError) . parseHeader) toHeader (Info "integer" (Just "int32"))
instance HasLeaf (Headers ctx) Int64     where leaf = Leaf (first (const ParseError) . parseHeader) toHeader (Info "integer" (Just "int64"))
instance HasLeaf (Headers ctx) Integer   where leaf = Leaf (first (const ParseError) . parseHeader) toHeader (Info "integer" Nothing)
instance HasLeaf (Headers ctx) Bool      where leaf = Leaf (first (const ParseError) . parseHeader) toHeader (Info "boolean" Nothing)
instance HasLeaf (Headers ctx) Float     where leaf = Leaf (first (const ParseError) . parseHeader) toHeader (Info "number" (Just "float"))
instance HasLeaf (Headers ctx) Double    where leaf = Leaf (first (const ParseError) . parseHeader) toHeader (Info "number" (Just "double"))
instance HasLeaf (Headers ctx) Text.Text where leaf = Leaf (first (const ParseError) . parseHeader) toHeader (Info "string" Nothing)
instance HasLeaf (Headers ctx) UUID      where leaf = Leaf (first (const ParseError) . parseHeader) toHeader (Info "string" (Just "uuid"))
instance HasLeaf (Headers ctx) Day       where leaf = Leaf (first (const ParseError) . parseHeader) toHeader (Info "string" (Just "date"))
instance HasLeaf (Headers ctx) LocalTime where leaf = Leaf (first (const ParseError) . parseHeader) toHeader (Info "string" (Just "date-time"))
instance HasLeaf (Headers ctx) UTCTime   where leaf = Leaf (first (const ParseError) . parseHeader) toHeader (Info "string" (Just "date-time"))
instance HasLeaf (Headers ctx) TimeOfDay where leaf = Leaf (first (const ParseError) . parseHeader) toHeader (Info "string" (Just "time"))

instance HasLeaf (Headers ctx) ByteString where
    leaf = Leaf Right id (Info "string" Nothing)

instance HasLeaf (Headers ctx) DiffTime where
    leaf = Leaf dec (encodeUtf8 . Text.pack . show . (realToFrac :: DiffTime -> Double)) (Info "number" Nothing)
      where
        dec bs = case (readMaybe (Text.unpack (decodeUtf8Lenient bs)) :: Maybe Double) of
            Just d  -> Right (realToFrac d)
            Nothing -> Left ParseError

instance HasLeaf (Headers ctx) (TimeOfDay, TimeZone) where
    leaf = Leaf dec enc (Info "string" (Just "time"))
      where
        enc (tod, tz) = encodeUtf8 (Text.pack (formatTime defaultTimeLocale "%T" tod <> timeZoneOffsetString tz))
        dec bs = case parseTimeM True defaultTimeLocale "%T%z" (Text.unpack (decodeUtf8Lenient bs)) of
            Just zt -> Right (localTimeOfDay (zonedTimeToLocalTime zt), zonedTimeZone zt)
            Nothing -> Left ParseError

class GHeaders ctx (f :: Type -> Type) where
    gHeadersCodec :: Tree (Headers ctx) (f ()) (f ())

instance (GHeaders ctx f) => GHeaders ctx (D1 meta f) where
    gHeadersCodec = FMap M1 $ LMap unM1 gHeadersCodec

instance (GHeaders ctx f) => GHeaders ctx (C1 meta f) where
    gHeadersCodec = FMap M1 $ LMap unM1 gHeadersCodec

instance (GHeaders ctx f, GHeaders ctx g) => GHeaders ctx (f :*: g) where
    gHeadersCodec =
        Apply
            (FMap (\l r -> l :*: r) (LMap (\(l :*: _) -> l) gHeadersCodec))
            (LMap (\(_ :*: r) -> r) gHeadersCodec)

instance (Selector s, HasLeaf (Headers ctx) a) => GHeaders ctx (S1 s (Rec0 a)) where
    gHeadersCodec =
        let key = fieldToHeaderName (selName (undefined :: S1 s (Rec0 a) ()))
         in FMap (M1 . K1) $ LMap (unK1 . unM1) $ Node (Field key (leaf @(Headers ctx) @a))

instance {-# OVERLAPPING #-} (Selector s, HasLeaf (Headers ctx) a) => GHeaders ctx (S1 s (Rec0 (Maybe a))) where
    gHeadersCodec =
        let key = fieldToHeaderName (selName (undefined :: S1 s (Rec0 (Maybe a)) ()))
         in FMap (M1 . K1) $ LMap (unK1 . unM1) $ Node (Field' key (leaf @(Headers ctx) @a))

instance {-# OVERLAPPING #-} (Selector s, KnownSymbol val) => GHeaders ctx (S1 s (Rec0 (ConstF val))) where
    gHeadersCodec =
        let k = fieldToHeaderName (selName (undefined :: S1 s (Rec0 (ConstF val)) ()))
            v = encodeUtf8 (Text.pack (symbolVal (Proxy @val)))
         in FMap (\() -> M1 (K1 ConstF)) $ LMap (const ()) $ Node (Field_ k v)

-- | Generically derive a 'Headers' codec from a record's field names and
--   'HasLeaf' instances (underscores become hyphens, e.g. field @x_a@
--   becomes header @x-a@).
--
-- >>> parser (derived @Page) [("x-a", "1"), ("x-b", "2")]
-- (Right (Page {x_a = 1, x_b = 2}),[])
-- >>> printer (derived @Page) (Page 1 2)
-- [("x-a","1"),("x-b","2")]
derived :: forall a ctx. (Generic a, GHeaders ctx (Rep a)) => Tree (Headers ctx) a a
derived = FMap (to @a) $ LMap (from @a) gHeadersCodec

-- $combined
-- >>> parser twoFields [("x-a", "1"), ("x-b", "2"), ("x-c", "3")]
-- (Right (1,2),[("x-c","3")])
--
-- prop> printParse parser printer twoFields (xy :: (Int, Int))
--
-- >>> parser twoCookies [("cookie", "sid=1; tok=2; other=3")]
-- (Right (1,2),[("cookie","other=3")])
--
-- prop> printParse parser printer twoCookies (xy :: (Int, Int))

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

data ConstF (val :: Symbol) = ConstF deriving (Eq, Show)

fieldToHeaderName :: String -> Types.HeaderName
fieldToHeaderName = CI.mk . encodeUtf8 . Text.pack . map (\c -> if c == '_' then '-' else c)
