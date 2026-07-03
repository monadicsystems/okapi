
module Okapi.HTTP.RFC9651.Dictionary (
    Dictionary,
    parser,
    printer,
    member,
    member',
    list,
    list',
    raw,
) where

import Data.ByteString (ByteString)
import Data.Kind (Type)
import Data.List (find)
import Okapi.Tree (Failure, Leaf, Parser, Printer, Context, Tree (..))
import Okapi.Tree qualified as Tree
import Okapi.HTTP.RFC9651.BareItem (BareItem, parseInnerToList, renderInner)
import Okapi.HTTP.RFC9651.Item (Item)
import Okapi.HTTP.RFC9651.Item qualified as Item
import Okapi.HTTP.RFC9651.Lexer (Key, ParseError (..), memberEntries)

type Dictionary :: Type -> Type
data Dictionary a where
    Member  :: Key -> Tree Item a a  -> Dictionary a
    Member' :: Key -> Tree Item a a  -> Dictionary (Maybe a)
    List    :: Key -> Leaf BareItem a -> Dictionary [a]
    List'   :: Key -> Leaf BareItem a -> Dictionary (Maybe [a])
    Raw     :: Dictionary ByteString

type instance Context Dictionary = ByteString
type instance Failure Dictionary = ParseError

member :: Key -> Tree Item a a -> Tree Dictionary a a
member k c = Node (Member k c)

member' :: Key -> Tree Item a a -> Tree Dictionary (Maybe a) (Maybe a)
member' k c = Node (Member' k c)

list :: Key -> Leaf BareItem a -> Tree Dictionary [a] [a]
list k vLeaf = Node (List k vLeaf)

list' :: Key -> Leaf BareItem a -> Tree Dictionary (Maybe [a]) (Maybe [a])
list' k vLeaf = Node (List' k vLeaf)

raw :: Tree Dictionary ByteString ByteString
raw = Node Raw

parser :: Tree Dictionary i o -> Parser Dictionary o
parser = Tree.parser alg
  where
    alg :: Dictionary a -> Parser Dictionary a
    alg t s = case t of
        Member key c -> case look s key of
            Just (Just v) -> (fst (Item.parser c v), s)
            Just Nothing  -> (fst (Item.parser c "?1"), s)
            Nothing       -> (Left ParseError, s)
        Member' key c -> case look s key of
            Just (Just v) -> (fmap Just (fst (Item.parser c v)), s)
            _             -> (Right Nothing, s)
        List key vLeaf -> case look s key of
            Just (Just v) -> (parseInnerToList vLeaf v, s)
            _             -> (Left ParseError, s)
        List' key vLeaf -> case look s key of
            Just (Just v) -> (fmap Just (parseInnerToList vLeaf v), s)
            _             -> (Right Nothing, s)
        Raw -> (Right s, s)
      where
        look bs k = fmap snd (find ((== k) . fst) (memberEntries bs))

printer :: Tree Dictionary i o -> Printer Dictionary i
printer = Tree.printer alg
  where
    alg :: Dictionary a -> Printer Dictionary a
    alg (Member key c)      a         = ", " <> key <> "=" <> Item.printer c a
    alg (Member' key c)     (Just a)  = ", " <> key <> "=" <> Item.printer c a
    alg (Member' _ _)       Nothing   = ""
    alg (List key vLeaf)    xs        = ", " <> key <> "=" <> renderInner vLeaf xs
    alg (List' key vLeaf)   (Just xs) = ", " <> key <> "=" <> renderInner vLeaf xs
    alg (List' _ _)         Nothing   = ""
    alg Raw                 bs        = bs
