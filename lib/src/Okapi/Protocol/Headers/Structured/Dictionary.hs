{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}

module Okapi.Protocol.Headers.Structured.Dictionary (
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
import Okapi.Codec (Codec (..), ParseErrorOf, StateOf)
import Okapi.Codec qualified as Codec
import Okapi.Data (Iso)
import Okapi.Protocol.Headers.Structured.BareItem (BareItem, parseInnerToList, renderInner)
import Okapi.Protocol.Headers.Structured.Item (Item, parseItem, printItem)
import Okapi.Protocol.Headers.Structured.Lexer (Key, ParseError (..), memberEntries)

type Dictionary :: Type -> Type
data Dictionary a where
    Member  :: Key -> Codec Item a a -> Dictionary a
    Member' :: Key -> Codec Item a a -> Dictionary (Maybe a)
    List  :: Key -> Iso BareItem a -> Dictionary [a]
    List' :: Key -> Iso BareItem a -> Dictionary (Maybe [a])
    Raw     :: Dictionary ByteString

type instance StateOf      Dictionary = ByteString
type instance ParseErrorOf Dictionary = ParseError

member :: Key -> Codec Item a a -> Codec Dictionary a a
member k c = Lift (Member k c)

member' :: Key -> Codec Item a a -> Codec Dictionary (Maybe a) (Maybe a)
member' k c = Lift (Member' k c)

list :: Key -> Iso BareItem a -> Codec Dictionary [a] [a]
list k vIso = Lift (List k vIso)

list' :: Key -> Iso BareItem a -> Codec Dictionary (Maybe [a]) (Maybe [a])
list' k vIso = Lift (List' k vIso)

raw :: Codec Dictionary ByteString ByteString
raw = Lift Raw

parseDictionary :: Codec Dictionary i o -> ByteString -> (Either ParseError o, ByteString)
parseDictionary = Codec.parser alg
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
        List key vIso -> case look s key of
            Just (Just v) -> (parseInnerToList vIso v, s)
            _             -> (Left ParseError, s)
        List' key vIso -> case look s key of
            Just (Just v) -> (fmap Just (parseInnerToList vIso v), s)
            _             -> (Right Nothing, s)
        Raw -> (Right s, s)
    look s k = fmap snd (find ((== k) . fst) (memberEntries s))

printDictionary :: Codec Dictionary i o -> i -> ByteString
printDictionary = Codec.printer pr
  where
    pr :: forall a. Dictionary a -> a -> ByteString
    pr (Member key c)     a         = ", " <> key <> "=" <> printItem c a
    pr (Member' key c)    (Just a)  = ", " <> key <> "=" <> printItem c a
    pr (Member' _ _)      Nothing   = ""
    pr (List key vIso)  xs        = ", " <> key <> "=" <> renderInner vIso xs
    pr (List' key vIso) (Just xs) = ", " <> key <> "=" <> renderInner vIso xs
    pr (List' _ _)      Nothing   = ""
    pr Raw                bs        = bs
