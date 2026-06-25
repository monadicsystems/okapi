{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}

module Okapi.Protocol.Headers.Structured.List (
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
import Okapi.Codec (Codec (..), ParseErrorOf, StateOf)
import Okapi.Codec qualified as Codec
import Okapi.Data (Iso)
import Okapi.Protocol.Headers.Structured.BareItem (BareItem, parseInnerToList, renderInner)
import Okapi.Protocol.Headers.Structured.Item (Item, parseItem, printItem)
import Okapi.Protocol.Headers.Structured.Lexer
    (ParseError (..), firstAndRest, splitTop, strip)

type List :: Type -> Type
data List a where
    ListItem  :: Codec Item a a -> List a
    InnerList :: Iso BareItem a -> List [a]
    Items     :: Codec Item a a -> List [a]
    Raw       :: List ByteString

type instance StateOf      List = ByteString
type instance ParseErrorOf List = ParseError

item :: Codec Item a a -> Codec List a a
item = Lift . ListItem

innerList :: Iso BareItem a -> Codec List [a] [a]
innerList = Lift . InnerList

items :: Codec Item a a -> Codec List [a] [a]
items = Lift . Items

raw :: Codec List ByteString ByteString
raw = Lift Raw

parseList :: Codec List i o -> ByteString -> (Either ParseError o, ByteString)
parseList = Codec.parser alg
  where
    alg :: forall a. List a -> ByteString -> (Either ParseError a, ByteString)
    alg t s = case t of
        ListItem c     -> let (m, rest) = firstAndRest 44 s
                          in (fst (parseItem c (strip m)), rest)
        InnerList vIso -> let (m, rest) = firstAndRest 44 s
                          in (parseInnerToList vIso (strip m), rest)
        Items c        -> (traverse (\m -> fst (parseItem c m)) (members s), BS.empty)
        Raw            -> (Right s, BS.empty)
    members bs = filter (not . BS.null) (map strip (splitTop 44 bs))

printList :: Codec List i o -> i -> ByteString
printList = Codec.printer pr
  where
    pr :: forall a. List a -> a -> ByteString
    pr (ListItem c)  a  = ", " <> printItem c a
    pr (InnerList vIso) xs = ", " <> renderInner vIso xs
    pr (Items c)     xs = BS.concat [", " <> printItem c x | x <- xs]
    pr Raw           bs = bs
