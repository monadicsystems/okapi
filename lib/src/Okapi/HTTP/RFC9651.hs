
module Okapi.HTTP.RFC9651 (
    RFC9651,
    ParseError (..),
    parser,
    printer,
    item,
    list,
    dictionary,
) where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Kind (Type)
import Data.Maybe (fromMaybe)
import Okapi.Tree (Failure, Parser, Printer, Context, Tree (..))
import Okapi.Tree qualified as Tree
import Okapi.HTTP.RFC9651.Dictionary (Dictionary)
import Okapi.HTTP.RFC9651.Dictionary qualified as Dictionary
import Okapi.HTTP.RFC9651.Item (Item)
import Okapi.HTTP.RFC9651.Item qualified as Item
import Okapi.HTTP.RFC9651.List (List)
import Okapi.HTTP.RFC9651.List qualified as List

data ParseError = ParseError deriving (Eq, Show)

strip :: ByteString -> ByteString
strip = BS.dropWhileEnd isSp . BS.dropWhile isSp
  where isSp w = w == 32 || w == 9

stripLead :: ByteString -> ByteString
stripLead bs = fromMaybe bs (BS.stripPrefix ", " bs)

type RFC9651 :: Type -> Type
data RFC9651 a where
    SItem :: Tree Item a a -> RFC9651 a
    SList :: Tree List a a -> RFC9651 a
    SDict :: Tree Dictionary a a -> RFC9651 a

type instance Context RFC9651 = ByteString
type instance Failure RFC9651 = ParseError

item :: Tree Item a a -> Tree RFC9651 a a
item = Node . SItem

list :: Tree List a a -> Tree RFC9651 a a
list = Node . SList

dictionary :: Tree Dictionary a a -> Tree RFC9651 a a
dictionary = Node . SDict

parser :: Tree RFC9651 i o -> Parser RFC9651 o
parser = Tree.parser alg
  where
    alg :: RFC9651 a -> Parser RFC9651 a
    alg (SItem c) s = case fst (Item.parser c (strip s)) of
        Left _  -> (Left ParseError, s)
        Right a -> (Right a, s)
    alg (SList c) s = case fst (List.parser c (strip s)) of
        Left _  -> (Left ParseError, s)
        Right a -> (Right a, s)
    alg (SDict c) s = case fst (Dictionary.parser c (strip s)) of
        Left _  -> (Left ParseError, s)
        Right a -> (Right a, s)

printer :: Tree RFC9651 i o -> Printer RFC9651 i
printer = Tree.printer alg
  where
    alg :: RFC9651 a -> Printer RFC9651 a
    alg (SItem c) a = Item.printer c a
    alg (SList c) a = stripLead (List.printer c a)
    alg (SDict c) a = stripLead (Dictionary.printer c a)
