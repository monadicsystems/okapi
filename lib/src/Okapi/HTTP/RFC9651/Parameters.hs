
module Okapi.HTTP.RFC9651.Parameters (
    Parameters,
    parser,
    printer,
    param,
    param',
    param_,
    flag,
    flag',
    raw,
) where

import Data.ByteString (ByteString)
import Data.Kind (Type)
import Data.List (find)
import Okapi.Tree (Failure, Leaf (..), Parser, Printer, Context, Tree (..))
import Okapi.Tree qualified as Tree
import Okapi.HTTP.RFC9651.BareItem (BareItem)
import Okapi.HTTP.RFC9651.Lexer (Key, ParseError (..), paramEntries)

type Parameters :: Type -> Type
data Parameters a where
    Param  :: Key -> Leaf BareItem a -> Parameters a
    Param' :: Key -> Leaf BareItem a -> Parameters (Maybe a)
    Param_ :: Key -> Leaf BareItem a -> a -> Parameters ()
    Flag   :: Key -> Parameters ()
    Flag'  :: Key -> Parameters Bool
    Raw    :: Parameters ByteString

type instance Context Parameters = ByteString
type instance Failure Parameters = ParseError

param :: Key -> Leaf BareItem a -> Tree Parameters a a
param k vLeaf = Node (Param k vLeaf)

param' :: Key -> Leaf BareItem a -> Tree Parameters (Maybe a) (Maybe a)
param' k vLeaf = Node (Param' k vLeaf)

param_ :: Key -> Leaf BareItem a -> a -> Tree Parameters () ()
param_ k vLeaf x = Node (Param_ k vLeaf x)

flag :: Key -> Tree Parameters () ()
flag = Node . Flag

flag' :: Key -> Tree Parameters Bool Bool
flag' = Node . Flag'

raw :: Tree Parameters ByteString ByteString
raw = Node Raw

parser :: Tree Parameters i o -> Parser Parameters o
parser = Tree.parser alg
  where
    alg :: Parameters a -> Parser Parameters a
    alg t s = case t of
        Param key vLeaf -> case look s key of
            Just (Just v) -> (vLeaf.decode v, s)
            _             -> (Left ParseError, s)
        Param' key vLeaf -> case look s key of
            Just (Just v) -> (Right (either (const Nothing) Just (vLeaf.decode v)), s)
            _             -> (Right Nothing, s)
        Param_ key vLeaf x -> case look s key of
            Just (Just v) | v == vLeaf.encode x -> (Right (), s)
            _                                   -> (Left ParseError, s)
        Flag key -> case look s key of
            Just Nothing      -> (Right (), s)
            Just (Just "?1")  -> (Right (), s)
            _                 -> (Left ParseError, s)
        Flag' key -> case look s key of
            Just Nothing     -> (Right True, s)
            Just (Just "?1") -> (Right True, s)
            _                -> (Right False, s)
        Raw -> (Right s, s)
      where
        look bs k = fmap snd (find ((== k) . fst) (paramEntries bs))

printer :: Tree Parameters i o -> Printer Parameters i
printer = Tree.printer alg
  where
    alg :: Parameters a -> Printer Parameters a
    alg (Param key vLeaf)    v        = ";" <> key <> "=" <> vLeaf.encode v
    alg (Param' key vLeaf)   (Just v) = ";" <> key <> "=" <> vLeaf.encode v
    alg (Param' _ _)         Nothing  = ""
    alg (Param_ key vLeaf x) ()       = ";" <> key <> "=" <> vLeaf.encode x
    alg (Flag key)           ()       = ";" <> key
    alg (Flag' key)          True     = ";" <> key
    alg (Flag' _)            False    = ""
    alg Raw                  bs       = bs
