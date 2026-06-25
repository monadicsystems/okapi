{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}

module Okapi.Protocol.Headers.Structured.Parameters (
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
import Okapi.Codec (Codec (..), ParseErrorOf, StateOf)
import Okapi.Codec qualified as Codec
import Okapi.Data (Iso (..))
import Okapi.Protocol.Headers.Structured.BareItem (BareItem)
import Okapi.Protocol.Headers.Structured.Lexer (Key, ParseError (..), paramEntries)

type Parameters :: Type -> Type
data Parameters a where
    Param  :: Key -> Iso BareItem a -> Parameters a
    Param' :: Key -> Iso BareItem a -> Parameters (Maybe a)
    Param_ :: Key -> Iso BareItem a -> a -> Parameters ()
    Flag   :: Key -> Parameters ()
    Flag'  :: Key -> Parameters Bool
    Raw    :: Parameters ByteString

type instance StateOf      Parameters = ByteString
type instance ParseErrorOf Parameters = ParseError

param :: Key -> Iso BareItem a -> Codec Parameters a a
param k vIso = Lift (Param k vIso)

param' :: Key -> Iso BareItem a -> Codec Parameters (Maybe a) (Maybe a)
param' k vIso = Lift (Param' k vIso)

param_ :: Key -> Iso BareItem a -> a -> Codec Parameters () ()
param_ k vIso x = Lift (Param_ k vIso x)

flag :: Key -> Codec Parameters () ()
flag = Lift . Flag

flag' :: Key -> Codec Parameters Bool Bool
flag' = Lift . Flag'

raw :: Codec Parameters ByteString ByteString
raw = Lift Raw

parseParameters :: Codec Parameters i o -> ByteString -> (Either ParseError o, ByteString)
parseParameters = Codec.parser alg
  where
    alg :: forall a. Parameters a -> ByteString -> (Either ParseError a, ByteString)
    alg t s = case t of
        Param key vIso -> case look s key of
            Just (Just v) -> (vIso.decode v, s)
            _             -> (Left ParseError, s)
        Param' key vIso -> case look s key of
            Just (Just v) -> (Right (either (const Nothing) Just (vIso.decode v)), s)
            _             -> (Right Nothing, s)
        Param_ key vIso x -> case look s key of
            Just (Just v) | v == vIso.encode x -> (Right (), s)
            _                                  -> (Left ParseError, s)
        Flag key -> case look s key of
            Just Nothing      -> (Right (), s)
            Just (Just "?1")  -> (Right (), s)
            _                 -> (Left ParseError, s)
        Flag' key -> case look s key of
            Just Nothing     -> (Right True, s)
            Just (Just "?1") -> (Right True, s)
            _                -> (Right False, s)
        Raw -> (Right s, s)
    look s k = fmap snd (find ((== k) . fst) (paramEntries s))

printParameters :: Codec Parameters i o -> i -> ByteString
printParameters = Codec.printer pr
  where
    pr :: forall a. Parameters a -> a -> ByteString
    pr (Param key vIso)  v        = ";" <> key <> "=" <> vIso.encode v
    pr (Param' key vIso) (Just v) = ";" <> key <> "=" <> vIso.encode v
    pr (Param' _ _)      Nothing  = ""
    pr (Param_ key vIso x) ()     = ";" <> key <> "=" <> vIso.encode x
    pr (Flag key)        ()       = ";" <> key
    pr (Flag' key)       True     = ";" <> key
    pr (Flag' _)         False    = ""
    pr Raw               bs       = bs
