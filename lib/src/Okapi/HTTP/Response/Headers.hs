{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module Okapi.HTTP.Response.Headers (
    Headers (..),
    ParseError (..),
    parser,
    printer,
    raw,
    field,
    field',
    field_,
    fieldRFC9651,
    parseExact,
    fieldBareItem,
    fieldItem,
    fieldList,
    fieldDictionary,
    contentType,
    setCookie,
    MediaType (..),
    ConstF (..),
    GHeaders (..),
    derived,
) where

import Data.Bifunctor (first)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
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
import GHC.TypeLits (KnownSymbol, symbolVal)
import Network.HTTP.Types qualified as HTTP
import Text.Read (readMaybe)
import Web.HttpApiData (parseHeader, toHeader)
import Okapi.Tree (Failure, HasLeaf (..), Info (..), Leaf (..), Parser, Printer, Piece, Context, Tree (..), widen)
import Okapi.Tree qualified as Tree
import Okapi.HTTP.Headers (MediaType (..), ConstF (..), fieldToHeaderName, mediaTypeBytes)
import Okapi.HTTP.Response.Attributes (Attributes)
import Okapi.HTTP.Response.Attributes qualified as Attributes
import Okapi.HTTP.Response.SetCookies (SetCookie)
import Okapi.HTTP.RFC9651 (RFC9651)
import Okapi.HTTP.RFC9651 qualified as RFC9651
import Okapi.HTTP.RFC9651.BareItem (BareItem)
import Okapi.HTTP.RFC9651.Item (Item, bareItem)
import Okapi.HTTP.RFC9651.List (List)
import Okapi.HTTP.RFC9651.Dictionary (Dictionary)

-- $setup
-- >>> :set -XApplicativeDo
-- >>> import Okapi.Tree (Leaf, printParse, int, integer, (=.))
-- >>> import Okapi.HTTP.Headers (MediaType (..))
-- >>> import Okapi.HTTP.RFC9651.BareItem (BareItem)
-- >>> import Okapi.HTTP.RFC9651.Dictionary qualified as Dictionary
-- >>> import Okapi.HTTP.RFC9651.Item qualified as Item
-- >>> import GHC.Generics (Generic)
-- >>> :{
-- let twoFields = do
--       a <- fst =. field "x-a" int
--       b <- snd =. field "x-b" int
--       pure (a, b)
-- :}
--
-- >>> :{
-- data Page = Page { x_a :: Int, x_b :: Int } deriving (Generic, Eq, Show)
-- :}

type Headers :: Type -> Type
data Headers a where
    Raw             :: Headers HTTP.RequestHeaders
    Field           :: HTTP.HeaderName -> Leaf Headers a -> Headers a
    Field'          :: HTTP.HeaderName -> Leaf Headers a -> Headers (Maybe a)
    Field_          :: HTTP.HeaderName -> ByteString -> Headers ()
    FieldRFC9651 :: HTTP.HeaderName -> Tree RFC9651 a a -> Headers a
    SetCookie       :: ByteString -> Leaf SetCookie a -> Tree Attributes p p -> Headers (a, p)

data ParseError = ParseError deriving (Eq, Show)

type instance Context Headers = [HTTP.Header]
type instance Failure Headers = ParseError
type instance Piece Headers = ByteString


parser :: Tree Headers i o -> Parser Headers o
parser = Tree.parser alg
  where
    alg :: Headers a -> Parser Headers a
    alg Raw hs = (Right hs, [])
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
    alg (FieldRFC9651 name c) hs =
        case partition (\(k, _) -> k == name) hs of
            ([], _)            -> (Left ParseError, hs)
            ((_, v) : _, rest) -> case RFC9651.parser c v of
                (Left _, _)              -> (Left ParseError, hs)
                (Right x, leftover)
                    | BS.null leftover  -> (Right x, rest)
                    | otherwise         -> (Right x, (name, leftover) : rest)
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

printer :: Tree Headers i o -> Printer Headers i
printer = Tree.printer alg
  where
    alg :: Headers a -> Printer Headers a
    alg Raw                           hs       = hs
    alg (Field key vLeaf)             x        = [(key, vLeaf.encode x)]
    alg (Field' _ _)                  Nothing  = []
    alg (Field' key vLeaf)            (Just x) = [(key, vLeaf.encode x)]
    alg (Field_ k v)                  ()       = [(k, v)]
    alg (FieldRFC9651 name c)         a        = [(name, RFC9651.printer c a)]
    alg (SetCookie name vLeaf attrsC) (x, p)   =
        [("set-cookie", name <> "=" <> vLeaf.encode x <> Attributes.printer attrsC p)]

-- | Require full consumption — 'Left' with the leftover headers (which may
--   include partially-consumed RFC 9651 values still under their original
--   header name, see 'fieldRFC9651') if any remain, 'Left' with the
--   underlying error if parsing itself failed.
parseExact :: Tree Headers i o -> [HTTP.Header] -> Either (Either ParseError [HTTP.Header]) o
parseExact = Tree.parseExact parser

-- | Pass all headers straight through, unconstrained.
--
-- >>> parser raw [("x-a", "1"), ("x-b", "2")]
-- (Right [("x-a","1"),("x-b","2")],[])
-- >>> printer raw [("x-a", "1"), ("x-b", "2")]
-- [("x-a","1"),("x-b","2")]
raw :: Tree Headers HTTP.RequestHeaders HTTP.RequestHeaders
raw = Node Raw

-- | Parse and print a required response header field.
--
-- prop> printParse parser printer (field "x-foo" int) (x :: Int)
field :: HTTP.HeaderName -> Leaf Headers a -> Tree Headers a a
field key vLeaf = Node (Field key vLeaf)

-- | Parse and print an optional response header field.
--
-- prop> printParse parser printer (field' "x-foo" int) (x :: Maybe Int)
field' :: HTTP.HeaderName -> Leaf Headers a -> Tree Headers (Maybe a) (Maybe a)
field' key vLeaf = Node (Field' key vLeaf)

-- | A required header field matched against a fixed literal value — same
--   shape as 'Okapi.HTTP.Request.Path.segment_': contributes @()@, and
--   ('widen'-built in) composes with siblings of /any/ type via ordinary
--   'Applicative'\/@do@-block sequencing, no explicit alignment needed —
--   not a wrapping combinator that takes "the rest of your headers" as an
--   explicit argument.
--
-- >>> parser (field_ "x-version" "2") [("x-version", "2")]
-- (Right (),[])
-- >>> parser (field_ "x-version" "2") [("x-version", "1")]
-- (Left ParseError,[("x-version","1")])
-- >>> printer (field_ "x-version" "2") ()
-- [("x-version","2")]
field_ :: HTTP.HeaderName -> ByteString -> Tree Headers i ()
field_ k v = widen (Node (Field_ k v))

-- | A header field whose entire value must be consumed by @c@ — any
--   unrecognized trailing content in the header is a parse error, not
--   silently dropped.
-- | A header field whose value is parsed\/printed via an RFC 9651 codec.
--   If @c@ doesn't consume the entire header value, the unrecognized
--   remainder is left in the context under the /same/ header name — same
--   discipline as 'field'\/'segment' leaving whatever they don't consume
--   for later combinators, just at the granularity of one header's value
--   instead of the whole context. A later combinator sharing that name
--   can continue parsing from exactly where this one left off (see
--   'fieldDictionary' for the motivating use case); use 'parseExact' if
--   you need to assert nothing was left over anywhere.
fieldRFC9651 :: HTTP.HeaderName -> Tree RFC9651 a a -> Tree Headers a a
fieldRFC9651 name c = Node (FieldRFC9651 name c)

-- | A header field whose value is parsed\/printed as an RFC 9651
--   Structured Field Value Item wrapping a single bare value. Trailing
--   content in the header value that @i@ doesn't recognize is left under
--   the same header name (see 'fieldRFC9651') rather than erroring —
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
fieldBareItem :: HTTP.HeaderName -> Leaf BareItem a -> Tree Headers a a
fieldBareItem name i = fieldRFC9651 name (RFC9651.item (bareItem i))

fieldItem :: HTTP.HeaderName -> Tree Item a a -> Tree Headers a a
fieldItem name c = fieldRFC9651 name (RFC9651.item c)

fieldList :: HTTP.HeaderName -> Tree List a a -> Tree Headers a a
fieldList name c = fieldRFC9651 name (RFC9651.list c)

-- | A header field whose value is parsed\/printed as an RFC 9651
--   Dictionary. Since 'fieldRFC9651' only consumes what @c@ recognizes,
--   two 'fieldDictionary's sharing the same header name — each matching a
--   different member — correctly split a single multi-member value across
--   two differently-typed record fields, combined via 'Okapi.Tree.Apply':
--
-- >>> :{
-- let two = (,) <$> (fst =. fieldDictionary "x" (Dictionary.member "a" (Item.bareItem (integer :: Leaf BareItem Integer))))
--               <*> (snd =. fieldDictionary "x" (Dictionary.member "b" (Item.bareItem (integer :: Leaf BareItem Integer))))
-- :}
--
-- >>> parser two [("x", "a=1, b=2")]
-- (Right (1,2),[])
fieldDictionary :: HTTP.HeaderName -> Tree Dictionary a a -> Tree Headers a a
fieldDictionary name c = fieldRFC9651 name (RFC9651.dictionary c)

-- | Asserts the @content-type@ header matches exactly — same shape as
--   'field_', see there.
--
-- >>> parser (contentType JSON) [("content-type", "application/json")]
-- (Right (),[])
contentType :: MediaType -> Tree Headers i ()
contentType mt = field_ "content-type" (mediaTypeBytes mt)

setCookie :: ByteString -> Leaf SetCookie a -> Tree Attributes p p -> Tree Headers (a, p) (a, p)
setCookie name vLeaf attrsC = Node (SetCookie name vLeaf attrsC)

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

-- | Generically derive a 'Headers' codec from a record's field names and
--   'HasLeaf' instances (underscores become hyphens, e.g. field @x_a@
--   becomes header @x-a@).
--
-- >>> parser (derived @Page) [("x-a", "1"), ("x-b", "2")]
-- (Right (Page {x_a = 1, x_b = 2}),[])
-- >>> printer (derived @Page) (Page 1 2)
-- [("x-a","1"),("x-b","2")]
derived :: forall a. (Generic a, GHeaders (Rep a)) => Tree Headers a a
derived = FMap (to @a) $ LMap (from @a) gHeadersCodec

-- $combined
-- >>> parser twoFields [("x-a", "1"), ("x-b", "2"), ("x-c", "3")]
-- (Right (1,2),[("x-c","3")])
--
-- prop> printParse parser printer twoFields (xy :: (Int, Int))
