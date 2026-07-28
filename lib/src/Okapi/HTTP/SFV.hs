
-- | Implements RFC 9651, "Structured Field Values for HTTP" (SFV).
module Okapi.HTTP.SFV (
    SFV,
    ParseError (..),
    parser,
    printer,
    parseExact,
    item,
    list,
    dict,
) where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Kind (Type)
import Data.Maybe (fromMaybe)
import Okapi.Tree (Failure, Parser, Printer, Context, Tree (..))
import Okapi.Tree qualified as Tree
import Okapi.HTTP.SFV.Dictionary (Dictionary)
import Okapi.HTTP.SFV.Dictionary qualified as Dictionary
import Okapi.HTTP.SFV.Item (Item)
import Okapi.HTTP.SFV.Item qualified as Item
import Okapi.HTTP.SFV.List (List)
import Okapi.HTTP.SFV.List qualified as List

-- $setup
-- >>> import Okapi.Tree (Leaf, printParse, parsePrintOr, integer, text, bool, (=.))
-- >>> import Okapi.HTTP.SFV.Item qualified as Item
-- >>> import Okapi.HTTP.SFV.List qualified as List
-- >>> import Okapi.HTTP.SFV.Dictionary qualified as Dictionary
-- >>> import Okapi.HTTP.SFV.Parameters qualified as Parameters
-- >>> import Okapi.HTTP.SFV.Bare (Bare, Token, token)
-- >>> import Data.ByteString (ByteString)
-- >>> import Data.ByteString.Char8 qualified as BS8
-- >>> import Data.Char (isSpace)
-- >>> import Data.Text (Text)
-- >>> import Test.QuickCheck.Instances ()
-- >>> import Test.QuickCheck (Gen, arbitrary, discard, forAll, frequency)
-- >>> let stripOWS = BS8.dropWhileEnd isSpace . BS8.dropWhile isSpace
-- >>> let mixedRFCItem = frequency [(1, arbitrary), (3, printer (item (Item.bareItem (integer :: Leaf Bare Integer))) <$> (arbitrary :: Gen Integer))] :: Gen ByteString
-- >>> let mixedRFCList = frequency [(1, arbitrary), (3, printer (list (List.items (Item.bareItem (integer :: Leaf Bare Integer)))) <$> (arbitrary :: Gen [Integer]))] :: Gen ByteString
-- >>> let mixedRFCDict = frequency [(1, arbitrary), (3, printer (dict (Dictionary.member "a" (Item.bareItem (integer :: Leaf Bare Integer)))) <$> (arbitrary :: Gen Integer))] :: Gen ByteString

data ParseError = ParseError deriving (Eq, Show)

strip :: ByteString -> ByteString
strip = BS.dropWhileEnd isSp . BS.dropWhile isSp
  where isSp w = w == 32 || w == 9

stripLead :: ByteString -> ByteString
stripLead bs = fromMaybe bs (BS.stripPrefix ", " bs)

-- | Inverse of 'stripLead', made idempotent: 'Dictionary' now requires its
--   own native form to always have a leading @,@ per entry (including the
--   first), but a fresh top-level header value never does. Only prepend
--   when it's not already there, so this is safe both for a fresh value
--   /and/ for a leftover previously produced by this same parser (already
--   in Dictionary's native form) being fed back in — see 'Okapi.Tree.parser'.
unstripLead :: ByteString -> ByteString
unstripLead bs = case BS.uncons bs of
    Just (44, _) -> bs
    _            -> if BS.null bs then bs else ", " <> bs

type SFV :: Type -> Type -> Type
data SFV i o where
    SItem :: Tree Item a a -> SFV a a
    SList :: Tree List a a -> SFV a a
    SDict :: Tree Dictionary a a -> SFV a a

type instance Context SFV = ByteString
type instance Failure SFV = ParseError

-- | A header value that is a single RFC 9651 Item (§3.3).
--
-- >>> parser (item (Item.bareItem (integer :: Leaf Bare Integer))) "5"
-- (Right 5,"")
-- >>> printer (item (Item.bareItem (integer :: Leaf Bare Integer))) 5
-- "5"
--
-- prop> printParse parser printer (item (Item.bareItem (integer :: Leaf Bare Integer))) (n :: Integer)
--
-- 'parser' strips leading\/trailing OWS as insignificant (correct — HTTP
-- treats it that way), but 'printer' never re-inserts arbitrary original
-- whitespace it was never told about, so the general 'parsePrint' claim
-- only holds once that same insignificant whitespace is stripped from the
-- tested input too — otherwise input like a single tab byte parses as
-- valid, empty content, and reprinting (naturally) doesn't reproduce the
-- tab:
--
-- prop> forAll mixedRFCItem (\bs -> parsePrintOr discard parser printer (item (Item.bareItem (integer :: Leaf Bare Integer))) (stripOWS bs))
item :: Tree Item a a -> Tree SFV a a
item = Node . SItem

-- | A header value that is a full RFC 9651 List (§3.3), e.g.
--   @sugar, tea, rum@ — the leading separator 'List.items' prints is
--   stripped here, so this is the correctly-canonical top-level form.
--
-- >>> parser (list (List.items (Item.bareItem (integer :: Leaf Bare Integer)))) "1, 2, 3"
-- (Right [1,2,3],"")
-- >>> printer (list (List.items (Item.bareItem (integer :: Leaf Bare Integer)))) [1,2,3]
-- "1, 2, 3"
--
-- prop> printParse parser printer (list (List.items (Item.bareItem (integer :: Leaf Bare Integer)))) (xs :: [Integer])
--
-- Same insignificant-OWS caveat as 'item' — see there.
--
-- prop> forAll mixedRFCList (\bs -> parsePrintOr discard parser printer (list (List.items (Item.bareItem (integer :: Leaf Bare Integer)))) (stripOWS bs))
--
-- RFC 9651 §3.1.1's own worked example — four hand-composed 'List.innerList'
-- members via 'Okapi.Tree.Apply' (including a trailing empty inner list),
-- confirming they really do chain correctly (each consuming its own
-- leading separator off what the previous one left behind, per
-- 'List.innerList''s own docs):
--
-- >>> let ils = List.innerList (text :: Leaf Bare Text)
-- >>> let four = (,,,) <$> ((\(a,_,_,_) -> a) =. ils) <*> ((\(_,b,_,_) -> b) =. ils) <*> ((\(_,_,c,_) -> c) =. ils) <*> ((\(_,_,_,d) -> d) =. ils)
-- >>> parser (list four) "(\"foo\" \"bar\"), (\"baz\"), (\"bat\" \"one\"), ()"
-- (Right (["foo","bar"],["baz"],["bat","one"],[]),"")
-- >>> printer (list four) (["foo","bar"],["baz"],["bat","one"],[])
-- "(\"foo\" \"bar\"), (\"baz\"), (\"bat\" \"one\"), ()"
--
-- RFC 9651 §3.1.1's other worked example — two inner lists, each with its
-- own list-level parameter (@lvl@), the first also a single item with
-- /its own/ two item-level parameters (@a@, @b@) via 'List.innerItem', the
-- second a plain two-item homogeneous inner list via 'List.innerItems':
--
-- >>> let tl = text :: Leaf Bare Text
-- >>> let il = integer :: Leaf Bare Integer
-- >>> let fooItem = Item.item tl ((,) <$> (fst =. Parameters.param "a" il) <*> (snd =. Parameters.param "b" il))
-- >>> let m1 = List.innerListOf (List.innerItem fooItem) (Parameters.param "lvl" il)
-- >>> let m2 = List.innerListOf (List.innerItems (Item.bareItem tl)) (Parameters.param "lvl" il)
-- >>> let nested = (,) <$> (fst =. m1) <*> (snd =. m2)
-- >>> parser (list nested) "(\"foo\"; a=1;b=2);lvl=5, (\"bar\" \"baz\");lvl=1"
-- (Right ((("foo",(1,2)),5),(["bar","baz"],1)),"")
-- >>> printer (list nested) ((("foo",(1,2)),5),(["bar","baz"],1))
-- "(\"foo\";a=1;b=2);lvl=5, (\"bar\" \"baz\");lvl=1"
list :: Tree List a a -> Tree SFV a a
list = Node . SList

-- | A header value that is a full RFC 9651 Dictionary (§3.3), e.g.
--   @a=1, b=2@ — same leading-separator stripping as 'list'. Wrapping a
--   single 'Dictionary.member' out of a larger multi-member value
--   correctly leaves the rest as leftover (in 'Dictionary''s own native,
--   leading-@,@ form) rather than silently discarding it — reparsing that
--   leftover under a /different/ member picks up exactly where the first
--   left off, which is what makes
--   'Okapi.HTTP.Headers.fieldDict' sound:
--
-- >>> parser (dict (Dictionary.member "a" (Item.bareItem (integer :: Leaf Bare Integer)))) "a=1"
-- (Right 1,"")
-- >>> parser (dict (Dictionary.member "a" (Item.bareItem (integer :: Leaf Bare Integer)))) "a=1, b=2"
-- (Right 1,", b=2")
-- >>> parser (dict (Dictionary.member "b" (Item.bareItem (integer :: Leaf Bare Integer)))) ", b=2"
-- (Right 2,"")
-- >>> printer (dict (Dictionary.member "a" (Item.bareItem (integer :: Leaf Bare Integer)))) 1
-- "a=1"
--
-- prop> printParse parser printer (dict (Dictionary.member "a" (Item.bareItem (integer :: Leaf Bare Integer)))) (n :: Integer)
--
-- Same insignificant-OWS caveat as 'item' — see there.
--
-- prop> forAll mixedRFCDict (\bs -> parsePrintOr discard parser printer (dict (Dictionary.member "a" (Item.bareItem (integer :: Leaf Bare Integer)))) (stripOWS bs))
--
-- The general 'printParseWith' with an /arbitrary/ trailing @extra@ does
-- /not/ hold here — concatenating a value's own printed bytes directly
-- against unstructured extra bytes (no separator in between) can corrupt
-- the leaf decode itself (e.g. appending @\'\\0\'@ right after @\"a=0\"@
-- doesn't parse back to @0@). It holds for the narrower, actually-relevant
-- case demonstrated above: @extra@ being a genuine leftover this same
-- parser already produced (i.e. already in 'Dictionary''s own
-- unambiguous, separator-delimited native form) — that's the situation
-- 'Okapi.HTTP.Headers.fieldDict' is actually in.
--
-- RFC 9651 §3.2's own worked example — @a@ is the explicit-false boolean
-- shorthand, @b@ is the bare-true shorthand, and @c@ is the bare-true
-- shorthand /with its own trailing parameter/ (the exact case that used to
-- break 'Dictionary.member' — see its 'Dictionary.breakKey'):
--
-- >>> let bl = bool :: Leaf Bare Bool
-- >>> let cVal = Item.item bl (Parameters.param "foo" (token :: Leaf Bare Token))
-- >>> let full = (,,) <$> ((\(a,_,_) -> a) =. Dictionary.member "a" (Item.bareItem bl)) <*> ((\(_,b,_) -> b) =. Dictionary.member "b" (Item.bareItem bl)) <*> ((\(_,_,c) -> c) =. Dictionary.member "c" cVal)
-- >>> parser (dict full) "a=?0, b, c; foo=bar"
-- (Right (False,True,(True,Token "bar")),"")
dict :: Tree Dictionary a a -> Tree SFV a a
dict = Node . SDict

parser :: Tree SFV i o -> Parser SFV o
parser = Tree.parser alg
  where
    alg :: SFV i o -> Parser SFV o
    alg (SItem c) s = case Item.parser c (strip s) of
        (Left _, _)     -> (Left ParseError, s)
        (Right a, rest) -> (Right a, rest)
    alg (SList c) s = case List.parser c (strip s) of
        (Left _, _)     -> (Left ParseError, s)
        (Right a, rest) -> (Right a, rest)
    alg (SDict c) s = case Dictionary.parser c (unstripLead (strip s)) of
        (Left _, _)     -> (Left ParseError, s)
        (Right a, rest) -> (Right a, rest)

printer :: Tree SFV i o -> Printer SFV i
printer = Tree.printer alg
  where
    alg :: SFV i o -> Printer SFV i
    alg (SItem c) a = Item.printer c a
    alg (SList c) a = stripLead (List.printer c a)
    alg (SDict c) a = stripLead (Dictionary.printer c a)

-- | Require full consumption — 'Left' with the leftover bytes if any
--   remain, 'Left' with the underlying error if parsing itself failed.
parseExact :: Tree SFV i o -> ByteString -> Either (Either ParseError ByteString) o
parseExact = Tree.parseExact parser
