
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
import Okapi.HTTP.RFC9651.BareItem (BareItem, parseInnerToList, renderInner)
import Okapi.HTTP.RFC9651.Item (Item)
import Okapi.HTTP.RFC9651.Item qualified as Item
import Okapi.HTTP.RFC9651.Lexer (ParseError (..), firstAndRest, splitTop, strip)

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
                           in (fst (Item.parser c (strip m)), rest)
        InnerList vLeaf -> let (m, rest) = firstAndRest 44 s
                           in (parseInnerToList vLeaf (strip m), rest)
        Items c         -> (traverse (\m -> fst (Item.parser c m)) (members s), BS.empty)
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
