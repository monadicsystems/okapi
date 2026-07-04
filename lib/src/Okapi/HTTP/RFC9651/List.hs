
module Okapi.HTTP.RFC9651.List (
    List,
    parser,
    printer,
    item,
    innerList,
    items,
    raw,
) where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Kind (Type)
import Okapi.Tree (Failure, Leaf, Parser, Printer, Context, Tree (..))
import Okapi.Tree qualified as Tree
import Data.Word (Word8)
import Okapi.HTTP.RFC9651.BareItem (BareItem, parseInnerToList, renderInner)
import Okapi.HTTP.RFC9651.Item (Item)
import Okapi.HTTP.RFC9651.Item qualified as Item

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

splitTop :: Word8 -> ByteString -> [ByteString]
splitTop sep bs = case firstTop sep bs of
    Nothing -> [bs]
    Just i  -> BS.take i bs : splitTop sep (BS.drop (i + 1) bs)

type List :: Type -> Type
data List a where
    ListItem  :: Tree Item a a -> List a
    InnerList :: Leaf BareItem a -> List [a]
    Items     :: Tree Item a a -> List [a]
    Raw       :: List ByteString

type instance Context List = ByteString
type instance Failure List = ParseError

item :: Tree Item a a -> Tree List a a
item = Node . ListItem

innerList :: Leaf BareItem a -> Tree List [a] [a]
innerList = Node . InnerList

items :: Tree Item a a -> Tree List [a] [a]
items = Node . Items

raw :: Tree List ByteString ByteString
raw = Node Raw

parser :: Tree List i o -> Parser List o
parser = Tree.parser alg
  where
    alg :: List a -> Parser List a
    alg t s = case t of
        ListItem c      -> let (m, rest) = firstAndRest 44 s
                           in case fst (Item.parser c (strip m)) of
                               Left _  -> (Left ParseError, rest)
                               Right a -> (Right a, rest)
        InnerList vLeaf -> let (m, rest) = firstAndRest 44 s
                           in case parseInnerToList vLeaf (strip m) of
                               Left _   -> (Left ParseError, rest)
                               Right xs -> (Right xs, rest)
        Items c         -> (traverse (\m -> case fst (Item.parser c m) of
                               Left _  -> Left ParseError
                               Right a -> Right a) (members s), BS.empty)
        Raw             -> (Right s, BS.empty)
      where
        members bs = filter (not . BS.null) (map strip (splitTop 44 bs))

printer :: Tree List i o -> Printer List i
printer = Tree.printer alg
  where
    alg :: List a -> Printer List a
    alg (ListItem c)      a  = ", " <> Item.printer c a
    alg (InnerList vLeaf) xs = ", " <> renderInner vLeaf xs
    alg (Items c)         xs = BS.concat [", " <> Item.printer c x | x <- xs]
    alg Raw               bs = bs
