
module Okapi.HTTP.Response.Headers.Attributes (
    Attributes,
    parser,
    printer,
    attr,
    attr',
    flag,
    flag',
    raw,
    maxAge,
    domain,
    path,
    secure,
    httpOnly,
) where

import Data.Bifunctor (first)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS8
import Data.Char (toLower)
import Data.Kind (Type)
import Okapi.HTTP.Tree (Failure, HasLeaf (..), Info (..), Leaf (..), Parser, Printer, Piece, Context, Tree (..))
import Okapi.HTTP.Tree qualified as Tree
import Web.HttpApiData (parseHeader, toHeader)

-- $setup
-- >>> :set -XApplicativeDo
-- >>> import Okapi.HTTP.Tree ((=.), HasLeaf (..), printParse, parsePrint)
-- >>> import Data.ByteString (ByteString)
-- >>> import Data.ByteString qualified as BS
-- >>> import Test.QuickCheck.Instances ()
-- >>> :{
-- let secureMaxAge = do
--       flag "Secure"
--       x <- maxAge
--       pure x
-- :}

type Attributes :: Type -> Type -> Type
data Attributes i o where
    Attr  :: ByteString -> Leaf Attributes a -> Attributes a a
    Attr' :: ByteString -> Leaf Attributes a -> Attributes (Maybe a) (Maybe a)
    Flag  :: ByteString -> Attributes i ()
    Flag' :: ByteString -> Attributes Bool Bool
    Raw   :: Attributes ByteString ByteString

data ParseError = ParseError deriving (Eq, Show)

type instance Piece Attributes = ByteString
type instance Context Attributes = ByteString
type instance Failure Attributes = ParseError

instance HasLeaf Attributes Int     where leaf = Leaf (first (const ParseError) . parseHeader) toHeader (Info "integer" Nothing)
instance HasLeaf Attributes Integer where leaf = Leaf (first (const ParseError) . parseHeader) toHeader (Info "integer" Nothing)
instance HasLeaf Attributes ByteString where leaf = Leaf Right id (Info "string" Nothing)

-- | A required, named attribute, e.g. the @Max-Age@ in @; Max-Age=100@. A
--   match removes the matched entry and re-serializes the rest as
--   leftover — combining several attributes via 'Okapi.HTTP.Tree.Apply'
--   correctly shrinks leftover step by step, entry by entry.
attr :: ByteString -> Leaf Attributes a -> Tree Attributes a a
attr key vLeaf = Node (Attr key vLeaf)

-- | An optional attribute — 'Nothing' when absent, printed as nothing.
--
-- >>> parser (attr' "Domain" (leaf @Attributes @ByteString)) "; Domain=example.com"
-- (Right (Just "example.com"),"")
-- >>> parser (attr' "Domain" (leaf @Attributes @ByteString)) ""
-- (Right Nothing,"")
-- >>> printer (attr' "Domain" (leaf @Attributes @ByteString)) (Just "example.com")
-- "; Domain=example.com"
-- >>> printer (attr' "Domain" (leaf @Attributes @ByteString)) Nothing
-- ""
--
-- The @Attributes@ 'HasLeaf' instance for 'ByteString' is a raw, unescaped
-- identity codec (unlike RFC 9651's quoted\/escaped @sf-string@) — a value
-- containing @;@ or leading\/trailing whitespace doesn't round-trip
-- through this unquoted, semicolon-delimited syntax (whitespace gets
-- stripped as if it were separator padding, and @;@ would be read back as
-- a new attribute boundary), so the law only holds for values that avoid
-- those bytes:
--
-- prop> \mbs -> printParse parser printer (attr' "Domain" (leaf @Attributes @ByteString)) (fmap (BS.filter (\w -> w /= 9 && w /= 32 && w /= 59)) mbs)
attr' :: ByteString -> Leaf Attributes a -> Tree Attributes (Maybe a) (Maybe a)
attr' key vLeaf = Node (Attr' key vLeaf)

-- | A boolean-valued attribute present with no value (bare @; a@) or
--   absent entirely — either is accepted, and printing always emits the
--   bare form.
--
-- >>> parser (flag "Secure") "; Secure"
-- (Right (),"")
-- >>> printer (flag "Secure") ()
-- "; Secure"
--
-- prop> printParse parser printer (flag "Secure") ()
--
-- Composes directly with a value-producing sibling in the same @do@\/
-- 'Applicative' block — no explicit alignment needed, since its own input
-- is unconstrained (see 'secureMaxAge' in "$setup"):
--
-- >>> parser secureMaxAge "; Secure; Max-Age=100"
-- (Right 100,"")
-- >>> printer secureMaxAge 100
-- "; Secure; Max-Age=100"
--
-- prop> printParse parser printer secureMaxAge (n :: Int)
flag :: ByteString -> Tree Attributes i ()
flag = Node . Flag

-- | Like 'flag', but observes whether the attribute was present as a
--   'Bool' instead of requiring it.
--
-- >>> parser (flag' "Secure") "; Secure"
-- (Right True,"")
-- >>> parser (flag' "Secure") ""
-- (Right False,"")
-- >>> printer (flag' "Secure") True
-- "; Secure"
-- >>> printer (flag' "Secure") False
-- ""
--
-- prop> printParse parser printer (flag' "Secure") (b :: Bool)
flag' :: ByteString -> Tree Attributes Bool Bool
flag' = Node . Flag'

-- | Pass the raw bytes straight through, unconstrained.
--
-- >>> parser raw "; whatever"
-- (Right "; whatever","")
-- >>> printer raw "; whatever"
-- "; whatever"
--
-- prop> printParse parser printer raw (bs :: ByteString)
raw :: Tree Attributes ByteString ByteString
raw = Node Raw

-- |
-- >>> parser maxAge "; Max-Age=100"
-- (Right 100,"")
-- >>> printer maxAge 100
-- "; Max-Age=100"
--
-- prop> printParse parser printer maxAge (n :: Int)
maxAge :: Tree Attributes Int Int
maxAge = attr "Max-Age" (leaf @Attributes @Int)

-- |
-- >>> parser domain "; Domain=example.com"
-- (Right "example.com","")
-- >>> printer domain "example.com"
-- "; Domain=example.com"
--
-- Combining two attributes via 'Okapi.HTTP.Tree.Apply' correctly shrinks the
-- leftover entry by entry, in either order:
--
-- >>> let both = (,) <$> (fst =. maxAge) <*> (snd =. domain)
-- >>> parser both "; Max-Age=100; Domain=example.com"
-- (Right (100,"example.com"),"")
-- >>> printer both (100, "example.com")
-- "; Max-Age=100; Domain=example.com"
--
-- Same unescaped-value caveat as 'attr'' — see there.
--
-- prop> \bs -> printParse parser printer domain (BS.filter (\w -> w /= 9 && w /= 32 && w /= 59) bs)
domain :: Tree Attributes ByteString ByteString
domain = attr "Domain" (leaf @Attributes @ByteString)

-- | Same unescaped-value caveat as 'attr'' — see there.
--
-- prop> \bs -> printParse parser printer path (BS.filter (\w -> w /= 9 && w /= 32 && w /= 59) bs)
path :: Tree Attributes ByteString ByteString
path = attr "Path" (leaf @Attributes @ByteString)

-- |
-- prop> printParse parser printer secure ()
secure :: Tree Attributes () ()
secure = flag "Secure"

-- |
-- prop> printParse parser printer httpOnly ()
httpOnly :: Tree Attributes () ()
httpOnly = flag "HttpOnly"

parser :: Tree Attributes i o -> Parser Attributes o
parser = Tree.parser alg
  where
    alg :: Attributes i o -> Parser Attributes o
    alg (Attr key vLeaf) s = case lookAndRemove key s of
        Just (Just v, rest) -> case vLeaf.decode v of
            Left _  -> (Left ParseError, s)
            Right x -> (Right x, rest)
        _ -> (Left ParseError, s)
    alg (Attr' key vLeaf) s = case lookAndRemove key s of
        Just (Just v, rest) -> case vLeaf.decode v of
            Right x -> (Right (Just x), rest)
            Left _  -> (Right Nothing, s)
        Just (Nothing, rest) -> (Right Nothing, rest)
        Nothing               -> (Right Nothing, s)
    alg (Flag key) s = case lookAndRemove key s of
        Just (_, rest) -> (Right (), rest)
        Nothing        -> (Left ParseError, s)
    alg (Flag' key) s = case lookAndRemove key s of
        Just (_, rest) -> (Right True, rest)
        Nothing        -> (Right False, s)
    alg Raw s = (Right s, BS.empty)

printer :: Tree Attributes i o -> Printer Attributes i
printer = Tree.printer alg
  where
    alg :: Attributes i o -> Printer Attributes i
    alg (Attr key vLeaf)  v        = "; " <> key <> "=" <> vLeaf.encode v
    alg (Attr' key vLeaf) (Just v) = "; " <> key <> "=" <> vLeaf.encode v
    alg (Attr' _ _)       Nothing  = ""
    alg (Flag key)        _        = "; " <> key
    alg (Flag' key)       True     = "; " <> key
    alg (Flag' _)         False    = ""
    alg Raw               bs       = bs

-- | Every well-formed 'Attributes' context is either empty or starts with
--   the literal @"; "@ ('printer' always prefixes exactly that) — checked
--   as a byte-for-byte prefix, not just a leading @;@, for the same reason
--   'Okapi.HTTP.Structured.Dictionary.memberEntries' does: a bare @;@ with no
--   following space is never actually produced by 'printer', so accepting
--   it here would let parsing succeed on bytes that don't reprint back to
--   themselves. A match removes the matched entry and re-serializes the
--   rest as leftover, so combining several attributes via
--   'Okapi.HTTP.Tree.Apply' correctly shrinks leftover entry by entry.
lookAndRemove :: ByteString -> ByteString -> Maybe (Maybe ByteString, ByteString)
lookAndRemove key bs = case BS.stripPrefix "; " bs of
    Nothing   -> Nothing
    Just rest -> case entries rest of
        Nothing -> Nothing
        Just es -> case break ((== lc key) . lc . fst) es of
            (_, [])             -> Nothing
            (before, e : after) -> Just (snd e, reserialize (before ++ after))
  where
    -- Any empty segment (a stray, doubled, or trailing @;@) is a real
    -- parse error, not silently collapsed — mirrors
    -- 'Okapi.HTTP.Structured.List' and 'Okapi.HTTP.Structured.Dictionary''s
    -- strict-separator treatment. Original casing is kept here (only
    -- lowercased transiently for the 'break' comparison above) —
    -- reserializing a lowercased key would change the leftover's bytes
    -- from what was actually in the input.
    entries s
        | any BS.null segs = Nothing
        | otherwise         = Just (map breakEq segs)
      where
        segs = map strip (BS.split 59 s)
    breakEq a = case BS.elemIndex 61 a of
        Nothing -> (a, Nothing)
        Just i  -> (strip (BS.take i a), Just (strip (BS.drop (i + 1) a)))
    reserialize = BS.concat . map renderAttrEntry
    renderAttrEntry (k, Nothing) = "; " <> k
    renderAttrEntry (k, Just v)  = "; " <> k <> "=" <> v

lc :: ByteString -> ByteString
lc = BS8.map toLower

strip :: ByteString -> ByteString
strip = BS.dropWhileEnd isSp . BS.dropWhile isSp
  where isSp w = w == 32 || w == 9
