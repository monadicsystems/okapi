{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}

module Okapi.HTTP.Headers.Structured.Item (
    Item,
    bareItem,
    bareItemEq,
    item,
    params,
    raw,
    parseItem,
    printItem,
) where

import Data.ByteString (ByteString)
import Data.Kind (Type)
import Okapi.Leaf (ErrorOf, Leaf (..), StateOf)
import Okapi.Tree (Tree (..), (=.))
import Okapi.Tree qualified as Tree
import Okapi.HTTP.Headers.Structured.BareItem (BareItem)
import Okapi.HTTP.Headers.Structured.Lexer (ParseError (..), firstAndRest, strip)
import Okapi.HTTP.Headers.Structured.Parameters (Parameters, parseParameters, printParameters)

type Item :: Type -> Type
data Item a where
    Bare   :: Leaf BareItem a -> Item a
    BareEq :: ByteString -> Item ()
    Params :: Tree Parameters p p -> Item p
    Raw    :: Item ByteString

type instance StateOf Item = ByteString
type instance ErrorOf Item = ParseError

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

parseItem :: Tree Item i o -> ByteString -> (Either ParseError o, ByteString)
parseItem = Tree.grow alg
  where
    alg :: forall a. Item a -> ByteString -> (Either ParseError a, ByteString)
    alg t s = case t of
        Bare vLeaf -> (vLeaf.decode (strip bare), s)
        BareEq c   -> (if strip bare == c then Right () else Left ParseError, s)
        Params c   -> (fst (parseParameters c ps), s)
        Raw        -> (Right s, s)
      where
        (bare, ps) = firstAndRest 59 s

printItem :: Tree Item i o -> i -> ByteString
printItem = Tree.eat pr
  where
    pr :: forall a. Item a -> a -> ByteString
    pr (Bare vLeaf) v  = vLeaf.encode v
    pr (BareEq c)   () = c
    pr (Params c)   p  = printParameters c p
    pr Raw          bs = bs
