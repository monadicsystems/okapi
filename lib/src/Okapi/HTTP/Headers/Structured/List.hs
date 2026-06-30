{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}

module Okapi.HTTP.Headers.Structured.List (
    List,
    item,
    innerList,
    items,
    raw,
    parseList,
    printList,
) where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Kind (Type)
import Okapi.Leaf (ErrorOf, Leaf, StateOf)
import Okapi.Tree (Tree (..))
import Okapi.Tree qualified as Tree
import Okapi.HTTP.Headers.Structured.BareItem (BareItem, parseInnerToList, renderInner)
import Okapi.HTTP.Headers.Structured.Item (Item, parseItem, printItem)
import Okapi.HTTP.Headers.Structured.Lexer (ParseError (..), firstAndRest, splitTop, strip)

type List :: Type -> Type
data List a where
    ListItem  :: Tree Item a a -> List a
    InnerList :: Leaf BareItem a -> List [a]
    Items     :: Tree Item a a -> List [a]
    Raw       :: List ByteString

type instance StateOf List = ByteString
type instance ErrorOf List = ParseError

item :: Tree Item a a -> Tree List a a
item = Node . ListItem

innerList :: Leaf BareItem a -> Tree List [a] [a]
innerList = Node . InnerList

items :: Tree Item a a -> Tree List [a] [a]
items = Node . Items

raw :: Tree List ByteString ByteString
raw = Node Raw

parseList :: Tree List i o -> ByteString -> (Either ParseError o, ByteString)
parseList = Tree.grow alg
  where
    alg :: forall a. List a -> ByteString -> (Either ParseError a, ByteString)
    alg t s = case t of
        ListItem c     -> let (m, rest) = firstAndRest 44 s
                          in (fst (parseItem c (strip m)), rest)
        InnerList vLeaf -> let (m, rest) = firstAndRest 44 s
                           in (parseInnerToList vLeaf (strip m), rest)
        Items c        -> (traverse (\m -> fst (parseItem c m)) (members s), BS.empty)
        Raw            -> (Right s, BS.empty)
    members bs = filter (not . BS.null) (map strip (splitTop 44 bs))

printList :: Tree List i o -> i -> ByteString
printList = Tree.eat pr
  where
    pr :: forall a. List a -> a -> ByteString
    pr (ListItem c)      a  = ", " <> printItem c a
    pr (InnerList vLeaf) xs = ", " <> renderInner vLeaf xs
    pr (Items c)         xs = BS.concat [", " <> printItem c x | x <- xs]
    pr Raw               bs = bs
