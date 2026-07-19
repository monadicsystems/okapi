
module Okapi.HTTP.Structured.Item (
    Item,
    parser,
    printer,
    parseExact,
    bareItem,
    bareItem_,
    item,
    params,
    raw,
) where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Kind (Type)
import Okapi.HTTP.Tree (Failure, Leaf (..), Parser, Printer, Context, Tree (..), (=.))
import Okapi.HTTP.Tree qualified as Tree
import Okapi.HTTP.Structured.BareItem (BareItem)
import Okapi.HTTP.Structured.Parameters (Parameters)
import Okapi.HTTP.Structured.Parameters qualified as Parameters
import Okapi.HTTP.Structured.Scan (strip, firstAndTail)

-- $setup
-- >>> :set -XApplicativeDo
-- >>> import Okapi.HTTP.Tree (Leaf, printParse, parsePrintOr, integer, text, bool, (=.))
-- >>> import Okapi.HTTP.Structured.Parameters qualified as Parameters
-- >>> import Okapi.HTTP.Structured.BareItem (BareItem, hasNonCanonicalInteger)
-- >>> import Data.Text (Text)
-- >>> import Data.ByteString (ByteString)
-- >>> import Data.ByteString.Char8 qualified as BS8
-- >>> import Test.QuickCheck.Instances ()
-- >>> import Test.QuickCheck (Gen, (==>), arbitrary, discard, forAll, frequency)
-- >>> let mixedInteger = frequency [(1, arbitrary), (3, BS8.pack . show <$> (arbitrary :: Gen Integer))] :: Gen ByteString
-- >>> let mixedItemBytes = frequency [(1, arbitrary), (3, printer (item (integer :: Leaf BareItem Integer) (Parameters.param "foo" (text :: Leaf BareItem Text))) <$> (arbitrary :: Gen (Integer, Text)))] :: Gen ByteString
-- >>> let mixedParamsBytes = frequency [(1, arbitrary), (3, printer (params (Parameters.param "a" (integer :: Leaf BareItem Integer))) <$> (arbitrary :: Gen Integer))] :: Gen ByteString
-- >>> :{
-- let taggedParam = do
--       bareItem_ "42"
--       x <- params (Parameters.param "a" (integer :: Leaf BareItem Integer))
--       pure x
-- :}

data ParseError = ParseError deriving (Eq, Show)

type Item :: Type -> Type -> Type
data Item i o where
    Bare   :: Leaf BareItem a -> Item a a
    BareEq :: ByteString -> Item i ()
    Params :: Tree Parameters p p -> Item p p
    Raw    :: Item ByteString ByteString

type instance Context Item = ByteString
type instance Failure Item = ParseError

-- | A bare item with no parameters.
--
-- >>> parser (bareItem (integer :: Leaf BareItem Integer)) "42"
-- (Right 42,"")
-- >>> printer (bareItem (integer :: Leaf BareItem Integer)) 42
-- "42"
--
-- prop> printParse parser printer (bareItem (integer :: Leaf BareItem Integer)) (n :: Integer)
-- prop> forAll mixedInteger (\bs -> not (hasNonCanonicalInteger bs) ==> parsePrintOr discard parser printer (bareItem (integer :: Leaf BareItem Integer)) bs)
bareItem :: Leaf BareItem a -> Tree Item a a
bareItem = Node . Bare

-- |
-- >>> parser (bareItem_ "42") "42"
-- (Right (),"")
-- >>> parser (bareItem_ "42") "43"
-- (Left ParseError,"43")
-- >>> printer (bareItem_ "42") ()
-- "42"
--
-- Composes directly with a value-producing sibling in the same @do@\/
-- 'Applicative' block — no explicit alignment needed, since its own input
-- is unconstrained. Its natural sibling is 'params' (the same shape
-- 'item' itself is built from, just with the bare value fixed instead of
-- decoded — see 'taggedParam' in "$setup"):
--
-- >>> parser taggedParam "42;a=1"
-- (Right 1,"")
-- >>> printer taggedParam 1
-- "42;a=1"
--
-- prop> printParse parser printer taggedParam (n :: Integer)
bareItem_ :: ByteString -> Tree Item i ()
bareItem_ = Node . BareEq

-- | A bare item followed by parameters, e.g. the RFC 9651 shape
--   @bare-item *parameters@ (§3.1) — @5;foo=\"bar\"@ is a bare integer @5@
--   with one parameter @foo=\"bar\"@. Unrecognized trailing parameters are
--   reported as leftover, not silently dropped:
--
-- >>> parser (item (integer :: Leaf BareItem Integer) (Parameters.param "foo" (text :: Leaf BareItem Text))) "5;foo=\"bar\""
-- (Right (5,"bar"),"")
-- >>> printer (item (integer :: Leaf BareItem Integer) (Parameters.param "foo" (text :: Leaf BareItem Text))) (5, "bar")
-- "5;foo=\"bar\""
-- >>> parser (item (integer :: Leaf BareItem Integer) (Parameters.param "foo" (text :: Leaf BareItem Text))) "5;foo=\"bar\";extra=1"
-- (Right (5,"bar"),";extra=1")
--
-- RFC 9651 §3.1.2's own worked example — @a@ present with no value is the
-- boolean-true shorthand (@a=?1@), @b@ has an explicit value:
--
-- >>> let ex = item (integer :: Leaf BareItem Integer) ((,) <$> (fst =. Parameters.flag' "a") <*> (snd =. Parameters.param' "b" (bool :: Leaf BareItem Bool)))
-- >>> parser ex "1; a; b=?0"
-- (Right (1,(True,Just False)),"")
--
-- prop> \n t -> printParse parser printer (item (integer :: Leaf BareItem Integer) (Parameters.param "foo" (text :: Leaf BareItem Text))) (n :: Integer, t :: Text)
-- prop> forAll mixedItemBytes (\bs -> not (hasNonCanonicalInteger bs) ==> parsePrintOr discard parser printer (item (integer :: Leaf BareItem Integer) (Parameters.param "foo" (text :: Leaf BareItem Text))) bs)
item :: Leaf BareItem a -> Tree Parameters p p -> Tree Item (a, p) (a, p)
item vLeaf c = (,) <$> (fst =. Node (Bare vLeaf)) <*> (snd =. Node (Params c))

-- | Parameters alone, without a preceding bare item. 'Params' forwards its
--   incoming context straight to 'Parameters.parser', so the input needs
--   to already be in @Parameters@'s own canonical, @;@-leading form (as it
--   would be when embedded after a bare item via 'item') — a context
--   missing that leading @;@ is now rejected outright, not leniently
--   accepted:
--
-- >>> parser (params (Parameters.param "a" (integer :: Leaf BareItem Integer))) ";a=1"
-- (Right 1,"")
-- >>> parser (params (Parameters.param "a" (integer :: Leaf BareItem Integer))) "a=1"
-- (Left ParseError,"a=1")
-- >>> printer (params (Parameters.param "a" (integer :: Leaf BareItem Integer))) 1
-- ";a=1"
--
-- prop> printParse parser printer (params (Parameters.param "a" (integer :: Leaf BareItem Integer))) (n :: Integer)
-- prop> forAll mixedParamsBytes (\bs -> not (hasNonCanonicalInteger bs) ==> parsePrintOr discard parser printer (params (Parameters.param "a" (integer :: Leaf BareItem Integer))) bs)
params :: Tree Parameters p p -> Tree Item p p
params = Node . Params

-- | Pass the raw bytes straight through, unconstrained.
--
-- >>> parser raw "5;foo=bar"
-- (Right "5;foo=bar","")
-- >>> printer raw "5;foo=bar"
-- "5;foo=bar"
--
-- prop> printParse parser printer raw (bs :: ByteString)
raw :: Tree Item ByteString ByteString
raw = Node Raw

parser :: Tree Item i o -> Parser Item o
parser = Tree.parser alg
  where
    alg :: Item i o -> Parser Item o
    alg (Bare vLeaf) s =
        let (bare, tl) = firstAndTail 59 s
        in case vLeaf.decode (strip bare) of
            Left _  -> (Left ParseError, s)
            Right a -> (Right a, tl)
    alg (BareEq c) s =
        let (bare, tl) = firstAndTail 59 s
        in if strip bare == c then (Right (), tl) else (Left ParseError, s)
    alg (Params c) s = case Parameters.parser c s of
        (Left _, _)     -> (Left ParseError, s)
        (Right p, rest) -> (Right p, rest)
    alg Raw s = (Right s, BS.empty)

printer :: Tree Item i o -> Printer Item i
printer = Tree.printer alg
  where
    alg :: Item i o -> Printer Item i
    alg (Bare vLeaf) v  = vLeaf.encode v
    alg (BareEq c)   _  = c
    alg (Params c)   p  = Parameters.printer c p
    alg Raw          bs = bs

-- | Require full consumption — 'Left' with the leftover bytes if any
--   remain, 'Left' with the underlying error if parsing itself failed.
parseExact :: Tree Item i o -> ByteString -> Either (Either ParseError ByteString) o
parseExact = Tree.parseExact parser
