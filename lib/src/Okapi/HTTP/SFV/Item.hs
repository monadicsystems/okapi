
module Okapi.HTTP.SFV.Item (
    Item,
    parser,
    printer,
    parseExact,
    bareItem,
    item,
    raw,
) where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Kind (Type)
import Okapi.Tree (Failure, Leaf (..), Parser, Printer, Context, Tree (..))
import Okapi.Tree qualified as Tree
import Okapi.HTTP.SFV.Bare (Bare)
import Okapi.HTTP.SFV.Parameters (Parameters)
import Okapi.HTTP.SFV.Parameters qualified as Parameters
import Okapi.HTTP.SFV.Scan (strip, firstAndTail)

-- $setup
-- >>> :set -XApplicativeDo
-- >>> import Okapi.Tree (Leaf, printParse, parsePrintOr, integer, text, bool, (=.))
-- >>> import Okapi.HTTP.SFV.Parameters qualified as Parameters
-- >>> import Okapi.HTTP.SFV.Bare (Bare, hasNonCanonicalInteger)
-- >>> import Data.Text (Text)
-- >>> import Data.ByteString (ByteString)
-- >>> import Data.ByteString.Char8 qualified as BS8
-- >>> import Test.QuickCheck.Instances ()
-- >>> import Test.QuickCheck (Gen, (==>), arbitrary, discard, forAll, frequency)
-- >>> let mixedInteger = frequency [(1, arbitrary), (3, BS8.pack . show <$> (arbitrary :: Gen Integer))] :: Gen ByteString
-- >>> let mixedItemBytes = frequency [(1, arbitrary), (3, printer (item (integer :: Leaf Bare Integer) (Parameters.param "foo" (text :: Leaf Bare Text))) <$> (arbitrary :: Gen (Integer, Text)))] :: Gen ByteString

data ParseError = ParseError deriving (Eq, Show)

type Item :: Type -> Type -> Type
data Item i o where
    Bare :: Leaf Bare a -> Item a a
    With :: Leaf Bare a -> Tree Parameters p p -> Item (a, p) (a, p)
    Raw  :: Item ByteString ByteString

type instance Context Item = ByteString
type instance Failure Item = ParseError

-- | A bare item with no parameters.
--
-- >>> parser (bareItem (integer :: Leaf Bare Integer)) "42"
-- (Right 42,"")
-- >>> printer (bareItem (integer :: Leaf Bare Integer)) 42
-- "42"
--
-- prop> printParse parser printer (bareItem (integer :: Leaf Bare Integer)) (n :: Integer)
-- prop> forAll mixedInteger (\bs -> not (hasNonCanonicalInteger bs) ==> parsePrintOr discard parser printer (bareItem (integer :: Leaf Bare Integer)) bs)
bareItem :: Leaf Bare a -> Tree Item a a
bareItem = Node . Bare

-- | A bare item followed by parameters, e.g. the RFC 9651 shape
--   @bare-item *parameters@ (§3.1) — @5;foo=\"bar\"@ is a bare integer @5@
--   with one parameter @foo=\"bar\"@. Unrecognized trailing parameters are
--   reported as leftover, not silently dropped:
--
-- >>> parser (item (integer :: Leaf Bare Integer) (Parameters.param "foo" (text :: Leaf Bare Text))) "5;foo=\"bar\""
-- (Right (5,"bar"),"")
-- >>> printer (item (integer :: Leaf Bare Integer) (Parameters.param "foo" (text :: Leaf Bare Text))) (5, "bar")
-- "5;foo=\"bar\""
-- >>> parser (item (integer :: Leaf Bare Integer) (Parameters.param "foo" (text :: Leaf Bare Text))) "5;foo=\"bar\";extra=1"
-- (Right (5,"bar"),";extra=1")
--
-- RFC 9651 §3.1.2's own worked example — @a@ present with no value is the
-- boolean-true shorthand (@a=?1@), @b@ has an explicit value:
--
-- >>> let ex = item (integer :: Leaf Bare Integer) ((,) <$> (fst =. Parameters.flag' "a") <*> (snd =. Parameters.param' "b" (bool :: Leaf Bare Bool)))
-- >>> parser ex "1; a; b=?0"
-- (Right (1,(True,Just False)),"")
--
-- prop> \n t -> printParse parser printer (item (integer :: Leaf Bare Integer) (Parameters.param "foo" (text :: Leaf Bare Text))) (n :: Integer, t :: Text)
-- prop> forAll mixedItemBytes (\bs -> not (hasNonCanonicalInteger bs) ==> parsePrintOr discard parser printer (item (integer :: Leaf Bare Integer) (Parameters.param "foo" (text :: Leaf Bare Text))) bs)
item :: Leaf Bare a -> Tree Parameters p p -> Tree Item (a, p) (a, p)
item vLeaf c = Node (With vLeaf c)

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
    alg (With vLeaf c) s =
        let (bare, tl) = firstAndTail 59 s
        in case vLeaf.decode (strip bare) of
            Left _  -> (Left ParseError, s)
            Right a -> case Parameters.parser c tl of
                (Left _, _)     -> (Left ParseError, s)
                (Right p, rest) -> (Right (a, p), rest)
    alg Raw s = (Right s, BS.empty)

printer :: Tree Item i o -> Printer Item i
printer = Tree.printer alg
  where
    alg :: Item i o -> Printer Item i
    alg (Bare vLeaf)   v      = vLeaf.encode v
    alg (With vLeaf c) (a, p) = vLeaf.encode a <> Parameters.printer c p
    alg Raw             bs    = bs

-- | Require full consumption — 'Left' with the leftover bytes if any
--   remain, 'Left' with the underlying error if parsing itself failed.
parseExact :: Tree Item i o -> ByteString -> Either (Either ParseError ByteString) o
parseExact = Tree.parseExact parser
