
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
import Data.List (find)
import Okapi.Tree (Failure, Leaf (..), Parser, Printer, Context, Tree (..))
import Okapi.Tree qualified as Tree
import Data.Word (Word8)
import Okapi.HTTP.RFC9651.BareItem (BareItem)

data ParseError = ParseError deriving (Eq, Show)

type Key = ByteString

strip :: ByteString -> ByteString
strip = BS.dropWhileEnd isSp . BS.dropWhile isSp
  where isSp w = w == 32 || w == 9

firstTop :: Word8 -> ByteString -> Maybe Int
firstTop sep bs = go False (0 :: Int) 0
  where
    n = BS.length bs
    go inQ depth i
        | i >= n = Nothing
        | otherwise =
            let w = BS.index bs i
            in if w == 34 then go (not inQ) depth (i + 1)
               else if inQ then go inQ depth (i + 1)
               else if w == 40 then go inQ (depth + 1) (i + 1)
               else if w == 41 then go inQ (max 0 (depth - 1)) (i + 1)
               else if w == sep && depth == 0 then Just i
               else go inQ depth (i + 1)

splitTop :: Word8 -> ByteString -> [ByteString]
splitTop sep bs = case firstTop sep bs of
    Nothing -> [bs]
    Just i  -> BS.take i bs : splitTop sep (BS.drop (i + 1) bs)

breakKey :: ByteString -> (ByteString, Maybe ByteString)
breakKey bs = case firstTop 61 bs of
    Nothing -> (strip bs, Nothing)
    Just i  -> (strip (BS.take i bs), Just (strip (BS.drop (i + 1) bs)))

paramEntries :: ByteString -> [(ByteString, Maybe ByteString)]
paramEntries = map breakKey . filter (not . BS.null) . map strip . splitTop 59

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

param :: Key -> Leaf BareItem a -> Tree Parameters a a
param k vLeaf = Node (Param k vLeaf)

param' :: Key -> Leaf BareItem a -> Tree Parameters (Maybe a) (Maybe a)
param' k vLeaf = Node (Param' k vLeaf)

param_ :: Key -> Leaf BareItem a -> a -> Tree Parameters () ()
param_ k vLeaf x = Node (Param_ k vLeaf x)

flag :: Key -> Tree Parameters () ()
flag = Node . Flag

flag' :: Key -> Tree Parameters Bool Bool
flag' = Node . Flag'

raw :: Tree Parameters ByteString ByteString
raw = Node Raw

parser :: Tree Parameters i o -> Parser Parameters o
parser = Tree.parser alg
  where
    alg :: Parameters a -> Parser Parameters a
    alg t s = case t of
        Param key vLeaf -> case look s key of
            Just (Just v) -> case vLeaf.decode v of
                Left _  -> (Left ParseError, s)
                Right a -> (Right a, s)
            _             -> (Left ParseError, s)
        Param' key vLeaf -> case look s key of
            Just (Just v) -> (Right (either (const Nothing) Just (vLeaf.decode v)), s)
            _             -> (Right Nothing, s)
        Param_ key vLeaf x -> case look s key of
            Just (Just v) | v == vLeaf.encode x -> (Right (), s)
            _                                   -> (Left ParseError, s)
        Flag key -> case look s key of
            Just Nothing      -> (Right (), s)
            Just (Just "?1")  -> (Right (), s)
            _                 -> (Left ParseError, s)
        Flag' key -> case look s key of
            Just Nothing     -> (Right True, s)
            Just (Just "?1") -> (Right True, s)
            _                -> (Right False, s)
        Raw -> (Right s, s)
      where
        look bs k = fmap snd (find ((== k) . fst) (paramEntries bs))

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
