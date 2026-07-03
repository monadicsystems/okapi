
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
import Okapi.HTTP.RFC9651.BareItem (BareItem)
import Okapi.HTTP.RFC9651.Lexer (ParseError (..), firstAndRest, strip)
import Okapi.HTTP.RFC9651.Parameters (Parameters)
import Okapi.HTTP.RFC9651.Parameters qualified as Parameters

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
        Bare vLeaf -> (vLeaf.decode (strip bare), s)
        BareEq c   -> (if strip bare == c then Right () else Left ParseError, s)
        Params c   -> (fst (Parameters.parser c ps), s)
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
