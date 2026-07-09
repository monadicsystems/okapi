
module Okapi.HTTP.RFC9651.Parameters (
    Parameters,
    parser,
    printer,
    param,
    param',
    param_,
    flag,
    flag',
    raw,
) where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Kind (Type)
import Okapi.Tree (Failure, Leaf (..), Parser, Printer, Context, Tree (..), widen)
import Okapi.Tree qualified as Tree
import Okapi.HTTP.RFC9651.BareItem (BareItem)
import Okapi.HTTP.RFC9651.Scan (strip, firstTop, splitTop)

-- $setup
-- >>> import Okapi.Tree (Leaf, printParse, parsePrint, parsePrintOr, integer)
-- >>> import Okapi.HTTP.RFC9651.BareItem (BareItem, hasNonCanonicalInteger)
-- >>> import Data.ByteString (ByteString)
-- >>> import Test.QuickCheck.Instances ()
-- >>> import Test.QuickCheck (Gen, (==>), arbitrary, discard, forAll, frequency)
-- >>> let mixedParamInteger = frequency [(1, arbitrary), (3, printer (param "a" (integer :: Leaf BareItem Integer)) <$> (arbitrary :: Gen Integer))] :: Gen ByteString
-- >>> let mixedFixedInteger = frequency [(1, arbitrary), (3, pure (printer (param_ "a" (integer :: Leaf BareItem Integer) 1) ()))] :: Gen ByteString
-- >>> let mixedFlag = frequency [(1, arbitrary), (3, pure (printer (flag "a") ()))] :: Gen ByteString

data ParseError = ParseError deriving (Eq, Show)

type Key = ByteString

breakKey :: ByteString -> (ByteString, Maybe ByteString)
breakKey bs = case firstTop 61 bs of
    Nothing -> (strip bs, Nothing)
    Just i  -> (strip (BS.take i bs), Just (strip (BS.drop (i + 1) bs)))

-- | Every well-formed 'Parameters' context is either empty or starts with
--   @;@ ('renderEntry' always prefixes it) — a non-empty context missing
--   that leading separator is malformed and reports as if it had no
--   entries at all, so every lookup then correctly fails as not-found.
paramEntries :: ByteString -> [(ByteString, Maybe ByteString)]
paramEntries bs = case BS.uncons bs of
    Nothing         -> []
    Just (59, rest) -> map breakKey (filter (not . BS.null) (map strip (splitTop 59 rest)))
    Just _          -> []

renderEntry :: (ByteString, Maybe ByteString) -> ByteString
renderEntry (k, Nothing) = ";" <> k
renderEntry (k, Just v)  = ";" <> k <> "=" <> v

reserialize :: [(ByteString, Maybe ByteString)] -> ByteString
reserialize = BS.concat . map renderEntry

-- | Like the old @look@, but also returns the remaining entries
--   re-serialized (matched entry removed) as the new leftover.
lookAndRemove :: ByteString -> Key -> Maybe (Maybe ByteString, ByteString)
lookAndRemove bs k = case break ((== k) . fst) (paramEntries bs) of
    (_, [])              -> Nothing
    (before, e : after)  -> Just (snd e, reserialize (before ++ after))

type Parameters :: Type -> Type
data Parameters a where
    Param  :: Key -> Leaf BareItem a -> Parameters a
    Param' :: Key -> Leaf BareItem a -> Parameters (Maybe a)
    Param_ :: Key -> Leaf BareItem a -> a -> Parameters ()
    Flag   :: Key -> Parameters ()
    Flag'  :: Key -> Parameters Bool
    Raw    :: Parameters ByteString

type instance Context Parameters = ByteString
type instance Failure Parameters = ParseError

-- | A required, named parameter, e.g. the @foo@ in @;foo=1@ (RFC 9651 §3.1.2).
--   A match removes the matched entry and re-serializes the rest as
--   leftover — combining several 'param's via 'Okapi.Tree.Apply' correctly
--   shrinks leftover step by step, entry by entry.
--
-- >>> parser (param "a" (integer :: Leaf BareItem Integer)) ";a=1;b=2"
-- (Right 1,";b=2")
-- >>> parser (param "a" (integer :: Leaf BareItem Integer)) "a=1"
-- (Left ParseError,"a=1")
-- >>> printer (param "a" (integer :: Leaf BareItem Integer)) 1
-- ";a=1"
--
-- prop> printParse parser printer (param "a" (integer :: Leaf BareItem Integer)) (n :: Integer)
-- prop> forAll mixedParamInteger (\bs -> not (hasNonCanonicalInteger bs) ==> parsePrintOr discard parser printer (param "a" (integer :: Leaf BareItem Integer)) bs)
param :: Key -> Leaf BareItem a -> Tree Parameters a a
param k vLeaf = Node (Param k vLeaf)

-- | An optional parameter — 'Nothing' when absent, printed as nothing.
--
-- >>> parser (param' "a" (integer :: Leaf BareItem Integer)) ";b=2"
-- (Right Nothing,";b=2")
-- >>> printer (param' "a" (integer :: Leaf BareItem Integer)) Nothing
-- ""
-- >>> printer (param' "a" (integer :: Leaf BareItem Integer)) (Just 1)
-- ";a=1"
--
-- prop> printParse parser printer (param' "a" (integer :: Leaf BareItem Integer)) (mn :: Maybe Integer)
-- prop> \bs -> not (hasNonCanonicalInteger bs) ==> parsePrint parser printer (param' "a" (integer :: Leaf BareItem Integer)) bs
param' :: Key -> Leaf BareItem a -> Tree Parameters (Maybe a) (Maybe a)
param' k vLeaf = Node (Param' k vLeaf)

-- | A parameter constrained to one known value — parses only if present
--   and equal to the fixed value, ignored on print (the value is baked in).
--
-- >>> parser (param_ "a" (integer :: Leaf BareItem Integer) 1) ";a=1"
-- (Right (),"")
-- >>> parser (param_ "a" (integer :: Leaf BareItem Integer) 1) ";a=2"
-- (Left ParseError,";a=2")
-- >>> printer (param_ "a" (integer :: Leaf BareItem Integer) 1) ()
-- ";a=1"
--
-- prop> printParse parser printer (param_ "a" (integer :: Leaf BareItem Integer) 1) ()
-- prop> forAll mixedFixedInteger (\bs -> not (hasNonCanonicalInteger bs) ==> parsePrintOr discard parser printer (param_ "a" (integer :: Leaf BareItem Integer) 1) bs)
param_ :: Key -> Leaf BareItem a -> a -> Tree Parameters i ()
param_ k vLeaf x = widen (Node (Param_ k vLeaf x))

-- | A boolean-valued parameter present with no value (bare @;a@) or absent
--   entirely — either is accepted, and printing always emits the bare form.
--
-- >>> parser (flag "a") ";a"
-- (Right (),"")
-- >>> printer (flag "a") ()
-- ";a"
--
-- prop> printParse parser printer (flag "a") ()
-- prop> forAll mixedFlag (\bs -> parsePrintOr discard parser printer (flag "a") bs)
flag :: Key -> Tree Parameters () ()
flag = Node . Flag

-- | Like 'flag', but observes whether the parameter was present as a 'Bool'
--   instead of requiring it.
--
-- >>> parser (flag' "a") ";a"
-- (Right True,"")
-- >>> parser (flag' "a") ""
-- (Right False,"")
-- >>> printer (flag' "a") True
-- ";a"
-- >>> printer (flag' "a") False
-- ""
--
-- prop> printParse parser printer (flag' "a") (b :: Bool)
-- prop> parsePrint parser printer (flag' "a") (bs :: ByteString)
flag' :: Key -> Tree Parameters Bool Bool
flag' = Node . Flag'

-- | Pass the raw bytes straight through, unconstrained.
--
-- >>> parser raw ";a=1;b=2"
-- (Right ";a=1;b=2","")
-- >>> printer raw ";a=1;b=2"
-- ";a=1;b=2"
--
-- prop> printParse parser printer raw (bs :: ByteString)
-- prop> parsePrint parser printer raw (bs :: ByteString)
raw :: Tree Parameters ByteString ByteString
raw = Node Raw

parser :: Tree Parameters i o -> Parser Parameters o
parser = Tree.parser alg
  where
    alg :: Parameters a -> Parser Parameters a
    alg (Param key vLeaf) s = case lookAndRemove s key of
        Just (Just v, rest) -> case vLeaf.decode v of
            Left _  -> (Left ParseError, s)
            Right a -> (Right a, rest)
        _ -> (Left ParseError, s)
    alg (Param' key vLeaf) s = case lookAndRemove s key of
        Just (Just v, rest) -> case vLeaf.decode v of
            Right a -> (Right (Just a), rest)
            Left _  -> (Right Nothing, s)
        _ -> (Right Nothing, s)
    alg (Param_ key vLeaf x) s = case lookAndRemove s key of
        Just (Just v, rest) | v == vLeaf.encode x -> (Right (), rest)
        _                                          -> (Left ParseError, s)
    alg (Flag key) s = case lookAndRemove s key of
        Just (Nothing, rest)   -> (Right (), rest)
        Just (Just "?1", rest) -> (Right (), rest)
        _                      -> (Left ParseError, s)
    alg (Flag' key) s = case lookAndRemove s key of
        Just (Nothing, rest)   -> (Right True, rest)
        Just (Just "?1", rest) -> (Right True, rest)
        Just (_, rest)         -> (Right False, rest)
        Nothing                -> (Right False, s)
    alg Raw s = (Right s, BS.empty)

printer :: Tree Parameters i o -> Printer Parameters i
printer = Tree.printer alg
  where
    alg :: Parameters a -> Printer Parameters a
    alg (Param key vLeaf)    v        = ";" <> key <> "=" <> vLeaf.encode v
    alg (Param' key vLeaf)   (Just v) = ";" <> key <> "=" <> vLeaf.encode v
    alg (Param' _ _)         Nothing  = ""
    alg (Param_ key vLeaf x) ()       = ";" <> key <> "=" <> vLeaf.encode x
    alg (Flag key)           ()       = ";" <> key
    alg (Flag' key)          True     = ";" <> key
    alg (Flag' _)            False    = ""
    alg Raw                  bs       = bs
