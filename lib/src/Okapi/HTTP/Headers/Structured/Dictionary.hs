{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}

module Okapi.HTTP.Headers.Structured.Dictionary (
    Dictionary,
    member,
    member',
    list,
    list',
    raw,
    parseDictionary,
    printDictionary,
) where

import Data.ByteString (ByteString)
import Data.Kind (Type)
import Data.List (find)
import Okapi.Leaf (ErrorOf, Leaf, StateOf)
import Okapi.Tree (Tree (..))
import Okapi.Tree qualified as Tree
import Okapi.HTTP.Headers.Structured.BareItem (BareItem, parseInnerToList, renderInner)
import Okapi.HTTP.Headers.Structured.Item (Item, parseItem, printItem)
import Okapi.HTTP.Headers.Structured.Lexer (Key, ParseError (..), memberEntries)

type Dictionary :: Type -> Type
data Dictionary a where
    Member  :: Key -> Tree Item a a  -> Dictionary a
    Member' :: Key -> Tree Item a a  -> Dictionary (Maybe a)
    List    :: Key -> Leaf BareItem a -> Dictionary [a]
    List'   :: Key -> Leaf BareItem a -> Dictionary (Maybe [a])
    Raw     :: Dictionary ByteString

type instance StateOf Dictionary = ByteString
type instance ErrorOf Dictionary = ParseError

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

parseDictionary :: Tree Dictionary i o -> ByteString -> (Either ParseError o, ByteString)
parseDictionary = Tree.grow alg
  where
    alg :: forall a. Dictionary a -> ByteString -> (Either ParseError a, ByteString)
    alg t s = case t of
        Member key c -> case look s key of
            Just (Just v) -> (fst (parseItem c v), s)
            Just Nothing  -> (fst (parseItem c "?1"), s)
            Nothing       -> (Left ParseError, s)
        Member' key c -> case look s key of
            Just (Just v) -> (fmap Just (fst (parseItem c v)), s)
            _             -> (Right Nothing, s)
        List key vLeaf -> case look s key of
            Just (Just v) -> (parseInnerToList vLeaf v, s)
            _             -> (Left ParseError, s)
        List' key vLeaf -> case look s key of
            Just (Just v) -> (fmap Just (parseInnerToList vLeaf v), s)
            _             -> (Right Nothing, s)
        Raw -> (Right s, s)
      where
        look s k = fmap snd (find ((== k) . fst) (memberEntries s))

printDictionary :: Tree Dictionary i o -> i -> ByteString
printDictionary = Tree.eat pr
  where
    pr :: forall a. Dictionary a -> a -> ByteString
    pr (Member key c)      a        = ", " <> key <> "=" <> printItem c a
    pr (Member' key c)     (Just a) = ", " <> key <> "=" <> printItem c a
    pr (Member' _ _)       Nothing  = ""
    pr (List key vLeaf)    xs       = ", " <> key <> "=" <> renderInner vLeaf xs
    pr (List' key vLeaf)   (Just xs)= ", " <> key <> "=" <> renderInner vLeaf xs
    pr (List' _ _)         Nothing  = ""
    pr Raw                 bs       = bs
