{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}

module Okapi.HTTP.Headers.Structured (
    Structured,
    ParseError (..),
    item,
    list,
    dictionary,
    parseStructured,
    printStructured,
) where

import Data.ByteString (ByteString)
import Data.Kind (Type)
import Okapi.Leaf (ErrorOf, StateOf)
import Okapi.Tree (Tree (..))
import Okapi.Tree qualified as Tree
import Okapi.HTTP.Headers.Structured.Dictionary (Dictionary, parseDictionary, printDictionary)
import Okapi.HTTP.Headers.Structured.Item (Item, parseItem, printItem)
import Okapi.HTTP.Headers.Structured.Lexer (ParseError (..), strip, stripLead)
import Okapi.HTTP.Headers.Structured.List (List, parseList, printList)

type Structured :: Type -> Type
data Structured a where
    SItem :: Tree Item a a -> Structured a
    SList :: Tree List a a -> Structured a
    SDict :: Tree Dictionary a a -> Structured a

type instance StateOf Structured = ByteString
type instance ErrorOf Structured = ParseError

item :: Tree Item a a -> Tree Structured a a
item = Node . SItem

list :: Tree List a a -> Tree Structured a a
list = Node . SList

dictionary :: Tree Dictionary a a -> Tree Structured a a
dictionary = Node . SDict

parseStructured :: Tree Structured i o -> ByteString -> (Either ParseError o, ByteString)
parseStructured = Tree.grow alg
  where
    alg :: forall a. Structured a -> ByteString -> (Either ParseError a, ByteString)
    alg (SItem c) s = (fst (parseItem c (strip s)), s)
    alg (SList c) s = (fst (parseList c (strip s)), s)
    alg (SDict c) s = (fst (parseDictionary c (strip s)), s)

printStructured :: Tree Structured i o -> i -> ByteString
printStructured = Tree.eat pr
  where
    pr :: forall a. Structured a -> a -> ByteString
    pr (SItem c) a = printItem c a
    pr (SList c) a = stripLead (printList c a)
    pr (SDict c) a = stripLead (printDictionary c a)
