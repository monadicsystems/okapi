
module Okapi.HTTP.RFC9651.Item (
    Item,
    parser,
    printer,
    parseExact,
    bareItem,
    bareItemEq,
    item,
    params,
    raw,
) where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Kind (Type)
import Okapi.Tree (Failure, Leaf (..), Parser, Printer, Context, Tree (..), (=.))
import Okapi.Tree qualified as Tree
import Okapi.HTTP.RFC9651.BareItem (BareItem)
import Okapi.HTTP.RFC9651.Parameters (Parameters)
import Okapi.HTTP.RFC9651.Parameters qualified as Parameters
import Okapi.HTTP.RFC9651.Scan (strip, firstAndTail)

-- $setup
-- >>> import Okapi.Tree (Leaf, printParse, parsePrintOr, integer, text, bool, (=.))
-- >>> import Okapi.HTTP.RFC9651.Parameters qualified as Parameters
-- >>> import Okapi.HTTP.RFC9651.BareItem (BareItem, hasNonCanonicalInteger)
-- >>> import Data.Text (Text)
-- >>> import Data.ByteString (ByteString)
-- >>> import Data.ByteString.Char8 qualified as BS8
-- >>> import Test.QuickCheck.Instances ()
-- >>> import Test.QuickCheck (Gen, (==>), arbitrary, discard, forAll, frequency)
-- >>> let mixedInteger = frequency [(1, arbitrary), (3, BS8.pack . show <$> (arbitrary :: Gen Integer))] :: Gen ByteString
-- >>> let mixedItemBytes = frequency [(1, arbitrary), (3, printer (item (integer :: Leaf BareItem Integer) (Parameters.param "foo" (text :: Leaf BareItem Text))) <$> (arbitrary :: Gen (Integer, Text)))] :: Gen ByteString
-- >>> let mixedParamsBytes = frequency [(1, arbitrary), (3, printer (params (Parameters.param "a" (integer :: Leaf BareItem Integer))) <$> (arbitrary :: Gen Integer))] :: Gen ByteString

data ParseError = ParseError deriving (Eq, Show)

type Item :: Type -> Type
data Item a where
    Bare   :: Leaf BareItem a -> Item a
    BareEq :: ByteString -> Item ()
    Params :: Tree Parameters p p -> Item p
    Raw    :: Item ByteString

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
-- >>> parser (bareItemEq "42") "42"
-- (Right (),"")
-- >>> parser (bareItemEq "42") "43"
-- (Left ParseError,"43")
-- >>> printer (bareItemEq "42") ()
-- "42"
bareItemEq :: ByteString -> Tree Item () ()
bareItemEq = Node . BareEq

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
    alg :: Item a -> Parser Item a
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
    alg :: Item a -> Printer Item a
    alg (Bare vLeaf) v  = vLeaf.encode v
    alg (BareEq c)   () = c
    alg (Params c)   p  = Parameters.printer c p
    alg Raw          bs = bs

-- | Require full consumption — 'Left' with the leftover bytes if any
--   remain, 'Left' with the underlying error if parsing itself failed.
parseExact :: Tree Item i o -> ByteString -> Either (Either ParseError ByteString) o
parseExact = Tree.parseExact parser
