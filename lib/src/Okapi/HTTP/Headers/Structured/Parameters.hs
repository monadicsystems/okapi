{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}

module Okapi.HTTP.Headers.Structured.Parameters (
    Parameters,
    param,
    param',
    param_,
    flag,
    flag',
    raw,
    parseParameters,
    printParameters,
) where

import Data.ByteString (ByteString)
import Data.Kind (Type)
import Data.List (find)
import Okapi.Leaf (ErrorOf, Leaf (..), StateOf)
import Okapi.Tree (Tree (..))
import Okapi.Tree qualified as Tree
import Okapi.HTTP.Headers.Structured.BareItem (BareItem)
import Okapi.HTTP.Headers.Structured.Lexer (Key, ParseError (..), paramEntries)

type Parameters :: Type -> Type
data Parameters a where
    Param  :: Key -> Leaf BareItem a -> Parameters a
    Param' :: Key -> Leaf BareItem a -> Parameters (Maybe a)
    Param_ :: Key -> Leaf BareItem a -> a -> Parameters ()
    Flag   :: Key -> Parameters ()
    Flag'  :: Key -> Parameters Bool
    Raw    :: Parameters ByteString

type instance StateOf Parameters = ByteString
type instance ErrorOf Parameters = ParseError

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

parseParameters :: Tree Parameters i o -> ByteString -> (Either ParseError o, ByteString)
parseParameters = Tree.grow alg
  where
    alg :: forall a. Parameters a -> ByteString -> (Either ParseError a, ByteString)
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
        look s k = fmap snd (find ((== k) . fst) (paramEntries s))

printParameters :: Tree Parameters i o -> i -> ByteString
printParameters = Tree.eat pr
  where
    pr :: forall a. Parameters a -> a -> ByteString
    pr (Param key vLeaf)    v        = ";" <> key <> "=" <> vLeaf.encode v
    pr (Param' key vLeaf)   (Just v) = ";" <> key <> "=" <> vLeaf.encode v
    pr (Param' _ _)         Nothing  = ""
    pr (Param_ key vLeaf x) ()       = ";" <> key <> "=" <> vLeaf.encode x
    pr (Flag key)           ()       = ";" <> key
    pr (Flag' key)          True     = ";" <> key
    pr (Flag' _)            False    = ""
    pr Raw                  bs       = bs
