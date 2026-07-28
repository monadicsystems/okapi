
module Okapi.HTTP.SFV.List (
    List,
    parser,
    printer,
    parseExact,
    item,
    innerList,
    items,
    raw,
    InnerItems,
    innerParser,
    innerPrinter,
    innerItem,
    innerItems,
    innerListOf,
) where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Kind (Type)
import Data.Maybe (fromMaybe)
import Okapi.Tree (Failure, Leaf, Parser, Printer, Context, Tree (..))
import Okapi.Tree qualified as Tree
import Okapi.HTTP.SFV.Bare (Bare, parseInnerToList, renderInner)
import Okapi.HTTP.SFV.Item (Item)
import Okapi.HTTP.SFV.Item qualified as Item
import Okapi.HTTP.SFV.Parameters (Parameters)
import Okapi.HTTP.SFV.Parameters qualified as Parameters
import Okapi.HTTP.SFV.Scan (strip, firstAndTail, firstTop, splitTop)

-- $setup
-- >>> import Okapi.Tree (Leaf, printParse, integer, text, (=.))
-- >>> import Okapi.HTTP.SFV.Item qualified as Item
-- >>> import Okapi.HTTP.SFV.Parameters qualified as Parameters
-- >>> import Okapi.HTTP.SFV.Bare (Bare)
-- >>> import Data.ByteString (ByteString)
-- >>> import Data.Text (Text)
-- >>> import Test.QuickCheck.Instances ()

data ParseError = ParseError deriving (Eq, Show)

type List :: Type -> Type -> Type
data List i o where
    ListItem    :: Tree Item a a -> List a a
    InnerList   :: Leaf Bare a -> List [a] [a]
    InnerListOf :: Tree InnerItems a a -> Tree Parameters p p -> List (a, p) (a, p)
    Items       :: Tree Item a a -> List [a] [a]
    Raw         :: List ByteString ByteString

type instance Context List = ByteString
type instance Failure List = ParseError

-- | The CONTENTS of an inner list, i.e. what's between its parens — RFC
--   9651 §3.1.1's @sf-innerlist@ members are full @sf-item@s (bare value
--   /plus/ optional parameters), space-separated, and — per the grammar —
--   never another inner list: there's no constructor for that here, so an
--   inner list can't nest inside an inner list by construction, the same
--   way 'Okapi.HTTP.SFV.Item.Item' has no \"I am an inner list\" case
--   either. Two shapes, mirroring 'item'\/'items' at the outer 'List'
--   level exactly: 'innerItem' (heterogeneous, one hand-composable element
--   at a time, for use with 'Okapi.Tree.Apply') and 'innerItems'
--   (homogeneous, a whole run of same-typed items in one pass).
type InnerItems :: Type -> Type -> Type
data InnerItems i o where
    InnerListItem  :: Tree Item a a -> InnerItems a a
    InnerListItems :: Tree Item a a -> InnerItems [a] [a]

type instance Context InnerItems = ByteString
type instance Failure InnerItems = ParseError

-- | One member of an RFC 9651 List (§3.1) — a single item, up to the next
--   top-level @,@. A per-element building block for use inside a bigger
--   combinator (see 'items'): printing always emits a leading @, @
--   separator (so that concatenating several printed items produces valid
--   @,@-joined list syntax), and parsing recognizes and drops that same
--   leading separator if present, leaving the /next/ entry's own leading
--   @,@ attached to the leftover — so a chain of 'item' parses over a
--   comma-joined sequence correctly consumes one entry at a time.
--
-- >>> parser (item (Item.bareItem (integer :: Leaf Bare Integer))) "1, 2, 3"
-- (Right 1,", 2, 3")
-- >>> printer (item (Item.bareItem (integer :: Leaf Bare Integer))) 1
-- ", 1"
--
-- prop> printParse parser printer (item (Item.bareItem (integer :: Leaf Bare Integer))) (n :: Integer)
item :: Tree Item a a -> Tree List a a
item = Node . ListItem

-- | One member of an RFC 9651 List that is itself an Inner List (§3.1.1),
--   e.g. @(1 2 3)@ — same per-element, leading-separator convention as
--   'item'.
--
-- >>> parser (innerList (integer :: Leaf Bare Integer)) "(1 2 3), 4"
-- (Right [1,2,3],", 4")
-- >>> printer (innerList (integer :: Leaf Bare Integer)) [1,2,3]
-- ", (1 2 3)"
--
-- prop> printParse parser printer (innerList (integer :: Leaf Bare Integer)) (xs :: [Integer])
innerList :: Leaf Bare a -> Tree List [a] [a]
innerList = Node . InnerList

-- | One member of an inner list's contents — a single 'Item' (bare value
--   /plus/ optional parameters), up to the next top-level space. Same
--   per-element, leading-separator convention as 'item' (there, a leading
--   @, @; here, a single leading space), for hand-composing a
--   heterogeneous sequence of differently-typed, individually-parameterized
--   items via 'Okapi.Tree.Apply' — see 'innerListOf'.
--
-- >>> innerParser (innerItem (Item.bareItem (integer :: Leaf Bare Integer))) "1 2 3"
-- (Right 1," 2 3")
-- >>> innerPrinter (innerItem (Item.bareItem (integer :: Leaf Bare Integer))) 1
-- " 1"
--
-- prop> printParse innerParser innerPrinter (innerItem (Item.bareItem (integer :: Leaf Bare Integer))) (n :: Integer)
innerItem :: Tree Item a a -> Tree InnerItems a a
innerItem = Node . InnerListItem

-- | A whole run of same-typed, individually-parameterized items, consumed
--   in one pass — the space-separated analogue of 'items'. Unlike
--   'innerList' (a single leaf value, no parameters), each element here is
--   a full 'Item', so e.g. @\"foo\"; a=1@ as one inner-list member is
--   representable.
--
-- >>> innerParser (innerItems (Item.bareItem (integer :: Leaf Bare Integer))) "1 2 3"
-- (Right [1,2,3],"")
-- >>> innerPrinter (innerItems (Item.bareItem (integer :: Leaf Bare Integer))) [1,2,3]
-- " 1 2 3"
--
-- prop> printParse innerParser innerPrinter (innerItems (Item.bareItem (integer :: Leaf Bare Integer))) (xs :: [Integer])
innerItems :: Tree Item a a -> Tree InnerItems [a] [a]
innerItems = Node . InnerListItems

-- | A List member that's an inner list built from 'innerItem'\/'innerItems'
--   (parameters-capable, and\/or heterogeneous), rather than 'innerList''s
--   bare-leaf-only shorthand — /and/, per RFC 9651 §3.1.1's @*parameters@
--   after the closing paren, parameters on the inner list itself, exactly
--   like 'Okapi.HTTP.SFV.Item.item' takes a bare value /plus/ a
--   parameters tree. Pass @pure ()@ for no list-level parameters. There's
--   no way to nest an inner list inside this — 'InnerItems' only ever
--   holds 'Okapi.HTTP.SFV.Item.Item's, by construction, matching RFC
--   9651 §3.1.1's restriction that inner lists can't nest. Requires the
--   wrapped 'InnerItems' tree to fully consume the parenthesized content
--   (too few or too many space-separated elements for what was asked for
--   is a parse error, not silently ignored) — unrecognized trailing
--   parameters, like 'Okapi.HTTP.SFV.Item.item', become leftover
--   rather than a hard error.
--
-- RFC 9651 §3.1.2 allows optional whitespace after each @;@ (@; a=1@ and
-- @;a=1@ are equivalent) — parsing accepts either, but printing always
-- emits the no-space canonical form:
--
-- >>> let itemWithParam = Item.item (text :: Leaf Bare Text) (Parameters.param "a" (integer :: Leaf Bare Integer))
-- >>> parser (innerListOf (innerItem itemWithParam) (pure ())) "(\"foo\"; a=1)"
-- (Right (("foo",1),()),"")
-- >>> printer (innerListOf (innerItem itemWithParam) (pure ())) (("foo",1),())
-- ", (\"foo\";a=1)"
--
-- With a real list-level parameter too — RFC 9651 §3.1.1's own worked
-- example, one inner list of it:
--
-- >>> parser (innerListOf (innerItem itemWithParam) (Parameters.param "lvl" (integer :: Leaf Bare Integer))) "(\"foo\"; a=1);lvl=5"
-- (Right (("foo",1),5),"")
-- >>> printer (innerListOf (innerItem itemWithParam) (Parameters.param "lvl" (integer :: Leaf Bare Integer))) (("foo",1),5)
-- ", (\"foo\";a=1);lvl=5"
--
-- prop> printParse parser printer (innerListOf (innerItems (Item.bareItem (integer :: Leaf Bare Integer))) (pure ())) (xs :: [Integer], ())
innerListOf :: Tree InnerItems a a -> Tree Parameters p p -> Tree List (a, p) (a, p)
innerListOf c ps = Node (InnerListOf c ps)

-- | A full RFC 9651 List of plain items (§3.1) — e.g. @sugar, tea, rum@,
--   built from a single leaf type. Unlike 'item'\/'innerList' (which parse
--   and print one entry at a time, for hand-composing a heterogeneous
--   sequence), this consumes the entire context in one pass (leftover is
--   always empty) — its leading-separator print convention is recognized
--   the same way 'item''s is, plus rejecting any other stray, doubled, or
--   trailing @,@ as a parse error.
--
-- >>> parser (items (Item.bareItem (integer :: Leaf Bare Integer))) "1, 2, 3"
-- (Right [1,2,3],"")
-- >>> printer (items (Item.bareItem (integer :: Leaf Bare Integer))) [1,2,3]
-- ", 1, 2, 3"
--
-- prop> printParse parser printer (items (Item.bareItem (integer :: Leaf Bare Integer))) (xs :: [Integer])
items :: Tree Item a a -> Tree List [a] [a]
items = Node . Items

-- | Pass the raw bytes straight through, unconstrained.
--
-- >>> parser raw "1, 2, 3"
-- (Right "1, 2, 3","")
-- >>> printer raw "1, 2, 3"
-- "1, 2, 3"
--
-- prop> printParse parser printer raw (bs :: ByteString)
raw :: Tree List ByteString ByteString
raw = Node Raw

parser :: Tree List i o -> Parser List o
parser = Tree.parser alg
  where
    alg :: List i o -> Parser List o
    alg t s = case t of
        ListItem c      -> let (m, rest) = firstAndTail 44 (stripLeadingSep s)
                           in case fst (Item.parser c (strip m)) of
                               Left _  -> (Left ParseError, rest)
                               Right a -> (Right a, rest)
        InnerList vLeaf -> let (m, rest) = firstAndTail 44 (stripLeadingSep s)
                           in case parseInnerToList vLeaf (strip m) of
                               Left _   -> (Left ParseError, rest)
                               Right xs -> (Right xs, rest)
        InnerListOf c ps -> let (m, rest) = firstAndTail 44 (stripLeadingSep s)
                            in case BS.uncons (strip m) of
                                Just (40, afterOpen) -> case firstTop 41 afterOpen of
                                    Just i ->
                                        let inner      = BS.take i afterOpen
                                            afterClose = BS.drop (i + 1) afterOpen
                                        in case Tree.parseExact innerParser c inner of
                                            Left _  -> (Left ParseError, s)
                                            Right a -> case Parameters.parser ps afterClose of
                                                (Left _, _)      -> (Left ParseError, s)
                                                (Right p, pRest) -> (Right (a, p), pRest <> rest)
                                    Nothing -> (Left ParseError, s)
                                _ -> (Left ParseError, s)
        Items c         -> case members s of
                               Nothing -> (Left ParseError, s)
                               Just ms -> (traverse (\m -> case fst (Item.parser c m) of
                                              Left _  -> Left ParseError
                                              Right a -> Right a) ms, BS.empty)
        Raw             -> (Right s, BS.empty)
      where
        -- Drop 'item'\/'innerList''s own leading @,@ separator convention
        -- (see their printers) if present, so a chain of successive parses
        -- over a comma-joined sequence each consume one entry and hand the
        -- next its own leading @,@ (via 'firstAndTail' keeping the /next/
        -- separator attached to the leftover) — matching content with no
        -- leading separator (a fresh, non-chained top-level string) is left
        -- untouched.
        stripLeadingSep bs = case BS.uncons (strip bs) of
            Just (44, rest) -> rest
            _               -> bs

        -- A literal leading @", "@ (checked byte-for-byte, /before/
        -- splitting) is 'printer''s own per-entry separator convention
        -- (every printed entry, including the first, starts with exactly
        -- @, @) and is dropped; checking post-split/stripped would lose
        -- the distinction between that and a bare stray @,@ with no space
        -- (both strip down to the same empty leading segment) — which is
        -- exactly the malformed case this must still reject. Any /other/
        -- empty segment is a stray, doubled, or trailing @,@ and is
        -- 'Nothing' (a real parse error) — except wholly empty input,
        -- which is the valid zero-item list. Mirrors
        -- 'Okapi.HTTP.SFV.Parameters' and
        -- 'Okapi.HTTP.SFV.Dictionary''s strict-separator treatment.
        members bs = case BS.stripPrefix ", " bs of
            Just rest -> checkSegs (splitTop 44 rest)
            Nothing
                | BS.null (strip bs) -> Just []
                | otherwise           -> checkSegs (splitTop 44 bs)
          where
            checkSegs raw
                | any BS.null segs = Nothing
                | otherwise        = Just segs
              where
                segs = map strip raw

printer :: Tree List i o -> Printer List i
printer = Tree.printer alg
  where
    alg :: List i o -> Printer List i
    alg (ListItem c)      a  = ", " <> Item.printer c a
    alg (InnerList vLeaf) xs = ", " <> renderInner vLeaf xs
    alg (InnerListOf c ps) (a, p) = ", (" <> fromMaybe content (BS.stripPrefix " " content) <> ")" <> Parameters.printer ps p
      where content = innerPrinter c a
    alg (Items c)         xs = BS.concat [", " <> Item.printer c x | x <- xs]
    alg Raw               bs = bs

-- | Require full consumption — 'Left' with the leftover bytes if any
--   remain, 'Left' with the underlying error if parsing itself failed.
parseExact :: Tree List i o -> ByteString -> Either (Either ParseError ByteString) o
parseExact = Tree.parseExact parser

-- | Parser for 'InnerItems' — the space-separated contents of an inner
--   list. Exported (alongside 'innerPrinter') so 'innerItem'\/'innerItems'
--   can be tested with the same round-trip law vocabulary as every other
--   DSL here, the way 'parser'\/'printer' let 'item'\/'items' be tested.
innerParser :: Tree InnerItems i o -> Parser InnerItems o
innerParser = Tree.parser alg
  where
    alg :: InnerItems i o -> Parser InnerItems o
    alg t s = case t of
        InnerListItem c  -> case nextItem s of
            Nothing         -> (Left ParseError, s)
            Just (m, rest) -> case fst (Item.parser c m) of
                Left _  -> (Left ParseError, s)
                Right a -> (Right a, rest)
        InnerListItems c -> case allItems s of
            Nothing -> (Left ParseError, s)
            Just ms -> (traverse (\m -> case fst (Item.parser c m) of
                           Left _  -> Left ParseError
                           Right a -> Right a) ms, BS.empty)
      where
        -- One item, plus zero or more of its own ";OWS parameter" groups,
        -- off the front of @bs@ — one leading space is dropped first if
        -- present (the chaining convention 'item'\/'stripLeadingSep'
        -- above also use). Uses 'itemEnd' rather than a plain top-level
        -- space split, since RFC 9651 allows OWS after each ";" (e.g.
        -- @; a=1@) — a naive split on the next top-level space would
        -- wrongly cut this item off in the middle of its own parameter
        -- list, right after that ";".
        nextItem bs0 =
            let bs = case BS.uncons bs0 of
                    Just (32, rest) -> rest
                    _               -> bs0
            in if BS.null bs
               then Nothing
               else let i    = itemEnd bs
                        m    = strip (BS.take i bs)
                        rest = BS.drop i bs
                    in if BS.null m then Nothing else Just (m, rest)

        -- Find where the CURRENT item ends: scan to the next top-level
        -- ";" or " ", whichever comes first. A ";" (plus whatever OWS
        -- follows it) is always still this item's own next parameter —
        -- RFC 9651's grammar only allows OWS /after/ ";", never before —
        -- so only a " " that isn't immediately (after skipping OWS) a ";"
        -- is a real item-to-item boundary.
        itemEnd bs = scanFrom 0
          where
            n = BS.length bs
            scanFrom from = case nextTop from of
                Nothing -> n
                Just i
                    | BS.index bs i == 59 -> scanFrom (skipOWS (i + 1))
                    | otherwise            -> i
            nextTop from =
                let sub = BS.drop from bs
                in case (firstTop 59 sub, firstTop 32 sub) of
                    (Nothing, Nothing)   -> Nothing
                    (Just si, Nothing)   -> Just (from + si)
                    (Nothing, Just spi)  -> Just (from + spi)
                    (Just si, Just spi)  -> Just (from + min si spi)
            skipOWS i
                | i < n, let w = BS.index bs i, w == 32 || w == 9 = skipOWS (i + 1)
                | otherwise = i

        allItems bs
            | BS.null (strip bs) = Just []
            | otherwise = case nextItem bs of
                Nothing        -> Nothing
                Just (m, rest)
                    | BS.null (strip rest) -> Just [m]
                    | otherwise             -> (m :) <$> allItems rest

innerPrinter :: Tree InnerItems i o -> Printer InnerItems i
innerPrinter = Tree.printer alg
  where
    alg :: InnerItems i o -> Printer InnerItems i
    alg (InnerListItem c)  a  = " " <> Item.printer c a
    alg (InnerListItems c) xs = BS.concat [" " <> Item.printer c x | x <- xs]
