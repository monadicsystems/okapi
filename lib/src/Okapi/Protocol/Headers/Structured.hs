{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}

module Okapi.Protocol.Headers.Structured (
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
import Okapi.Codec (Codec (..), ParseErrorOf, StateOf)
import Okapi.Codec qualified as Codec
import Okapi.Protocol.Headers.Structured.Dictionary (Dictionary, parseDictionary, printDictionary)
import Okapi.Protocol.Headers.Structured.Item (Item, parseItem, printItem)
import Okapi.Protocol.Headers.Structured.Lexer (ParseError (..), strip, stripLead)
import Okapi.Protocol.Headers.Structured.List (List, parseList, printList)

type Structured :: Type -> Type
data Structured a where
    SItem :: Codec Item a a -> Structured a
    SList :: Codec List a a -> Structured a
    SDict :: Codec Dictionary a a -> Structured a

type instance StateOf      Structured = ByteString
type instance ParseErrorOf Structured = ParseError

item :: Codec Item a a -> Codec Structured a a
item = Lift . SItem

list :: Codec List a a -> Codec Structured a a
list = Lift . SList

dictionary :: Codec Dictionary a a -> Codec Structured a a
dictionary = Lift . SDict

parseStructured :: Codec Structured i o -> ByteString -> (Either ParseError o, ByteString)
parseStructured = Codec.parser alg
  where
    alg :: forall a. Structured a -> ByteString -> (Either ParseError a, ByteString)
    alg (SItem c) s = (fst (parseItem c (strip s)), s)
    alg (SList c) s = (fst (parseList c (strip s)), s)
    alg (SDict c) s = (fst (parseDictionary c (strip s)), s)

printStructured :: Codec Structured i o -> i -> ByteString
printStructured = Codec.printer pr
  where
    pr :: forall a. Structured a -> a -> ByteString
    pr (SItem c) a = printItem c a
    pr (SList c) a = stripLead (printList c a)
    pr (SDict c) a = stripLead (printDictionary c a)
