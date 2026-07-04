
module Okapi.HTTP.RFC9651.Item (
    Item,
    parser,
    printer,
    bareItem,
    bareItemEq,
    item,
    params,
    raw,
) where

import Data.ByteString (ByteString)
import Data.Kind (Type)
import Okapi.Tree (Failure, Leaf (..), Parser, Printer, Context, Tree (..), (=.))
import Okapi.Tree qualified as Tree
import Data.ByteString qualified as BS
import Data.Word (Word8)
import Okapi.HTTP.RFC9651.BareItem (BareItem)
import Okapi.HTTP.RFC9651.Parameters (Parameters)
import Okapi.HTTP.RFC9651.Parameters qualified as Parameters

data ParseError = ParseError deriving (Eq, Show)

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

firstAndRest :: Word8 -> ByteString -> (ByteString, ByteString)
firstAndRest sep bs = case firstTop sep bs of
    Nothing -> (bs, BS.empty)
    Just i  -> (BS.take i bs, BS.drop (i + 1) bs)

type Item :: Type -> Type
data Item a where
    Bare   :: Leaf BareItem a -> Item a
    BareEq :: ByteString -> Item ()
    Params :: Tree Parameters p p -> Item p
    Raw    :: Item ByteString

type instance Context Item = ByteString
type instance Failure Item = ParseError

bareItem :: Leaf BareItem a -> Tree Item a a
bareItem = Node . Bare

bareItemEq :: ByteString -> Tree Item () ()
bareItemEq = Node . BareEq

item :: Leaf BareItem a -> Tree Parameters p p -> Tree Item (a, p) (a, p)
item vLeaf c = (,) <$> (fst =. Node (Bare vLeaf)) <*> (snd =. Node (Params c))

params :: Tree Parameters p p -> Tree Item p p
params = Node . Params

raw :: Tree Item ByteString ByteString
raw = Node Raw

parser :: Tree Item i o -> Parser Item o
parser = Tree.parser alg
  where
    alg :: Item a -> Parser Item a
    alg t s = case t of
        Bare vLeaf -> case vLeaf.decode (strip bare) of
                          Left _  -> (Left ParseError, s)
                          Right a -> (Right a, s)
        BareEq c   -> (if strip bare == c then Right () else Left ParseError, s)
        Params c   -> case fst (Parameters.parser c ps) of
                          Left _  -> (Left ParseError, s)
                          Right p -> (Right p, s)
        Raw        -> (Right s, s)
      where
        (bare, ps) = firstAndRest 59 s

printer :: Tree Item i o -> Printer Item i
printer = Tree.printer alg
  where
    alg :: Item a -> Printer Item a
    alg (Bare vLeaf) v  = vLeaf.encode v
    alg (BareEq c)   () = c
    alg (Params c)   p  = Parameters.printer c p
    alg Raw          bs = bs
