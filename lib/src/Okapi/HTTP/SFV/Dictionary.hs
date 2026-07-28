
module Okapi.HTTP.SFV.Dictionary (
    Dictionary,
    parser,
    printer,
    parseExact,
    member,
    member',
    list,
    list',
    raw,
) where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Kind (Type)
import Data.Word (Word8)
import Okapi.Tree (Failure, Leaf, Parser, Printer, Context, Tree (..))
import Okapi.Tree qualified as Tree
import Okapi.HTTP.SFV.Bare (Bare, parseInnerToList, renderInner)
import Okapi.HTTP.SFV.Item (Item)
import Okapi.HTTP.SFV.Item qualified as Item
import Okapi.HTTP.SFV.Scan (strip, splitTop)

-- $setup
-- >>> import Okapi.Tree (Leaf, printParse, parsePrint, parsePrintOr, integer)
-- >>> import Okapi.HTTP.SFV.Item qualified as Item
-- >>> import Okapi.HTTP.SFV.Bare (Bare, hasNonCanonicalInteger)
-- >>> import Data.ByteString (ByteString)
-- >>> import Test.QuickCheck.Instances ()
-- >>> import Test.QuickCheck (Gen, (==>), arbitrary, discard, forAll, frequency)
-- >>> let mixedMemberInteger = frequency [(1, arbitrary), (3, printer (member "a" (Item.bareItem (integer :: Leaf Bare Integer))) <$> (arbitrary :: Gen Integer))] :: Gen ByteString
-- >>> let mixedListInteger = frequency [(1, arbitrary), (3, printer (list "a" (integer :: Leaf Bare Integer)) <$> (arbitrary :: Gen [Integer]))] :: Gen ByteString

data ParseError = ParseError deriving (Eq, Show)

type Key = ByteString

-- | RFC 9651 §3.1: @key = lcalpha *( lcalpha \/ DIGIT \/ "_" \/ "-" \/ "." \/ "*" )@.
isKeyChar :: Word8 -> Bool
isKeyChar w = isLcAlpha w || isDigit w || w `BS.elem` "_-.*"
  where
    isLcAlpha c = c >= 0x61 && c <= 0x7a
    isDigit c = c >= 0x30 && c <= 0x39

-- | An explicit @key=value@ (@value@ is the raw text after @=@, itself
--   possibly followed by its own @;parameters@ — 'Item.parser' handles
--   that), or a bare @key@ possibly followed by @;parameters@ on the
--   RFC 9651 §3.2 implicit-@?1@ value — @params@ here is that trailing
--   text verbatim (empty, or starting with @;@), not yet combined with
--   the @?1@ substitution (each of 'Member'\/'Member''\/'List'\/'List''
--   uses this differently: only 'Member'\/'Member'' can sensibly default
--   to a boolean).
data EntryValue = Explicit ByteString | Implicit ByteString
    deriving (Eq, Show)

-- | Finds the key using its own grammar (a restricted character class),
--   not by searching for the first @=@ anywhere in the entry — the first
--   @=@ can belong to a trailing parameter instead (@c; foo=bar@, RFC 9651
--   §3.2's own worked example: the key is @c@, bare, with @; foo=bar@ as
--   parameters on its implicit @?1@ value; blindly splitting on the first
--   @=@ would wrongly take @"c; foo"@ as the key and @"bar"@ as the value).
breakKey :: ByteString -> (ByteString, EntryValue)
breakKey bs = case BS.span isKeyChar bs of
    (key, rest) -> case BS.uncons rest of
        Just (0x3d, valuePart) -> (key, Explicit (strip valuePart))
        _                      -> (key, Implicit rest)

-- | Every well-formed 'Dictionary' context is either empty or starts with
--   the literal @", "@ ('renderEntry' always prefixes exactly that, comma
--   /and/ space) — checked as a byte-for-byte prefix rather than just a
--   leading comma, since a bare @,@ with no following space would
--   otherwise be accepted here but never actually produced by 'renderEntry',
--   breaking the forward round trip (parse succeeds, but reprinting
--   normalizes to @", "@ and no longer matches the original bytes). A
--   non-empty context missing that leading separator is malformed and
--   reports as if it had no entries at all, so every lookup then correctly
--   fails as not-found.
memberEntries :: ByteString -> [(ByteString, EntryValue)]
memberEntries bs = case BS.stripPrefix ", " bs of
    Just rest -> map breakKey (filter (not . BS.null) (map strip (splitTop 44 rest)))
    Nothing   -> []

renderEntry :: (ByteString, EntryValue) -> ByteString
renderEntry (k, Explicit v)  = ", " <> k <> "=" <> v
renderEntry (k, Implicit ps) = ", " <> k <> ps

reserialize :: [(ByteString, EntryValue)] -> ByteString
reserialize = BS.concat . map renderEntry

-- | Like the old @look@, but also returns the remaining entries
--   re-serialized (matched entry removed) as the new leftover.
lookAndRemove :: ByteString -> Key -> Maybe (EntryValue, ByteString)
lookAndRemove bs k = case break ((== k) . fst) (memberEntries bs) of
    (_, [])              -> Nothing
    (before, e : after)  -> Just (snd e, reserialize (before ++ after))

type Dictionary :: Type -> Type -> Type
data Dictionary i o where
    Member  :: Key -> Tree Item a a  -> Dictionary a a
    Member' :: Key -> Tree Item a a  -> Dictionary (Maybe a) (Maybe a)
    List    :: Key -> Leaf Bare a -> Dictionary [a] [a]
    List'   :: Key -> Leaf Bare a -> Dictionary (Maybe [a]) (Maybe [a])
    Raw     :: Dictionary ByteString ByteString

type instance Context Dictionary = ByteString
type instance Failure Dictionary = ParseError

-- | A required, named entry, e.g. the RFC 9651 Dictionary (§3.2) member
--   @a@ in @, a=1, b=2@ (the internal, always-leading-separator
--   representation — see 'Okapi.HTTP.SFV.dict' for the
--   stripped, canonical top-level header-value form). A bare key with no
--   value is treated as the boolean shorthand @a=?1@. A match removes the
--   matched entry and re-serializes the rest as leftover — combining
--   several 'member's via 'Okapi.Tree.Apply' correctly shrinks leftover
--   entry by entry.
--
-- >>> parser (member "a" (Item.bareItem (integer :: Leaf Bare Integer))) ", a=1, b=2"
-- (Right 1,", b=2")
-- >>> parser (member "a" (Item.bareItem (integer :: Leaf Bare Integer))) "a=1, b=2"
-- (Left ParseError,"a=1, b=2")
-- >>> printer (member "a" (Item.bareItem (integer :: Leaf Bare Integer))) 1
-- ", a=1"
--
-- prop> printParse parser printer (member "a" (Item.bareItem (integer :: Leaf Bare Integer))) (n :: Integer)
-- prop> forAll mixedMemberInteger (\bs -> not (hasNonCanonicalInteger bs) ==> parsePrintOr discard parser printer (member "a" (Item.bareItem (integer :: Leaf Bare Integer))) bs)
member :: Key -> Tree Item a a -> Tree Dictionary a a
member k c = Node (Member k c)

-- | An optional entry — 'Nothing' when absent, printed as nothing.
--
-- >>> parser (member' "a" (Item.bareItem (integer :: Leaf Bare Integer))) ", b=2"
-- (Right Nothing,", b=2")
-- >>> printer (member' "a" (Item.bareItem (integer :: Leaf Bare Integer))) Nothing
-- ""
-- >>> printer (member' "a" (Item.bareItem (integer :: Leaf Bare Integer))) (Just 1)
-- ", a=1"
--
-- prop> printParse parser printer (member' "a" (Item.bareItem (integer :: Leaf Bare Integer))) (mn :: Maybe Integer)
-- prop> \bs -> not (hasNonCanonicalInteger bs) ==> parsePrint parser printer (member' "a" (Item.bareItem (integer :: Leaf Bare Integer))) bs
member' :: Key -> Tree Item a a -> Tree Dictionary (Maybe a) (Maybe a)
member' k c = Node (Member' k c)

-- | A required entry whose value is an Inner List, e.g. @a=(1 2 3)@.
--
-- >>> parser (list "a" (integer :: Leaf Bare Integer)) ", a=(1 2 3), b=2"
-- (Right [1,2,3],", b=2")
-- >>> printer (list "a" (integer :: Leaf Bare Integer)) [1,2,3]
-- ", a=(1 2 3)"
--
-- prop> printParse parser printer (list "a" (integer :: Leaf Bare Integer)) (xs :: [Integer])
-- prop> forAll mixedListInteger (\bs -> not (hasNonCanonicalInteger bs) ==> parsePrintOr discard parser printer (list "a" (integer :: Leaf Bare Integer)) bs)
list :: Key -> Leaf Bare a -> Tree Dictionary [a] [a]
list k vLeaf = Node (List k vLeaf)

-- | Optional version of 'list'.
--
-- >>> parser (list' "a" (integer :: Leaf Bare Integer)) ", b=2"
-- (Right Nothing,", b=2")
-- >>> printer (list' "a" (integer :: Leaf Bare Integer)) Nothing
-- ""
--
-- prop> printParse parser printer (list' "a" (integer :: Leaf Bare Integer)) (mxs :: Maybe [Integer])
-- prop> \bs -> not (hasNonCanonicalInteger bs) ==> parsePrint parser printer (list' "a" (integer :: Leaf Bare Integer)) bs
list' :: Key -> Leaf Bare a -> Tree Dictionary (Maybe [a]) (Maybe [a])
list' k vLeaf = Node (List' k vLeaf)

-- | Pass the raw bytes straight through, unconstrained.
--
-- >>> parser raw "a=1, b=2"
-- (Right "a=1, b=2","")
-- >>> printer raw "a=1, b=2"
-- "a=1, b=2"
--
-- prop> printParse parser printer raw (bs :: ByteString)
-- prop> parsePrint parser printer raw (bs :: ByteString)
raw :: Tree Dictionary ByteString ByteString
raw = Node Raw

parser :: Tree Dictionary i o -> Parser Dictionary o
parser = Tree.parser alg
  where
    alg :: Dictionary i o -> Parser Dictionary o
    alg (Member key c) s = case lookAndRemove s key of
        Just (Explicit v, rest) -> case fst (Item.parser c v) of
            Left _  -> (Left ParseError, s)
            Right a -> (Right a, rest)
        Just (Implicit ps, rest) -> case fst (Item.parser c ("?1" <> ps)) of
            Left _  -> (Left ParseError, s)
            Right a -> (Right a, rest)
        Nothing -> (Left ParseError, s)
    alg (Member' key c) s = case lookAndRemove s key of
        Just (Explicit v, rest) -> case fst (Item.parser c v) of
            Left _  -> (Right Nothing, s)
            Right a -> (Right (Just a), rest)
        Just (Implicit ps, rest) -> case fst (Item.parser c ("?1" <> ps)) of
            Left _  -> (Right Nothing, s)
            Right a -> (Right (Just a), rest)
        Nothing -> (Right Nothing, s)
    alg (List key vLeaf) s = case lookAndRemove s key of
        Just (Explicit v, rest) -> case parseInnerToList vLeaf v of
            Left _   -> (Left ParseError, s)
            Right xs -> (Right xs, rest)
        _ -> (Left ParseError, s)
    alg (List' key vLeaf) s = case lookAndRemove s key of
        Just (Explicit v, rest) -> case parseInnerToList vLeaf v of
            Left _   -> (Right Nothing, s)
            Right xs -> (Right (Just xs), rest)
        _ -> (Right Nothing, s)
    alg Raw s = (Right s, BS.empty)

printer :: Tree Dictionary i o -> Printer Dictionary i
printer = Tree.printer alg
  where
    alg :: Dictionary i o -> Printer Dictionary i
    alg (Member key c)      a         = ", " <> key <> "=" <> Item.printer c a
    alg (Member' key c)     (Just a)  = ", " <> key <> "=" <> Item.printer c a
    alg (Member' _ _)       Nothing   = ""
    alg (List key vLeaf)    xs        = ", " <> key <> "=" <> renderInner vLeaf xs
    alg (List' key vLeaf)   (Just xs) = ", " <> key <> "=" <> renderInner vLeaf xs
    alg (List' _ _)         Nothing   = ""
    alg Raw                 bs        = bs

-- | Require full consumption — 'Left' with the leftover bytes if any
--   remain, 'Left' with the underlying error if parsing itself failed.
parseExact :: Tree Dictionary i o -> ByteString -> Either (Either ParseError ByteString) o
parseExact = Tree.parseExact parser
