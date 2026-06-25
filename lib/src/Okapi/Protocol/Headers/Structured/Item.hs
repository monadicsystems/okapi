{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}

module Okapi.Protocol.Headers.Structured.Item (
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
import Okapi.Codec (Codec (..), ParseErrorOf, StateOf, (=.))
import Okapi.Codec qualified as Codec
import Okapi.Data (Iso (..))
import Okapi.Protocol.Headers.Structured.BareItem (BareItem)
import Okapi.Protocol.Headers.Structured.Lexer (ParseError (..), firstAndRest, strip)
import Okapi.Protocol.Headers.Structured.Parameters (Parameters, parseParameters, printParameters)

type Item :: Type -> Type
data Item a where
  Bare   :: Iso BareItem a -> Item a
  BareEq :: ByteString -> Item ()
  Params :: Codec Parameters p p -> Item p
  Raw    :: Item ByteString

type instance StateOf Item = ByteString
type instance ParseErrorOf Item = ParseError

bareItem :: Iso BareItem a -> Codec Item a a
bareItem = Lift . Bare

bareItemEq :: ByteString -> Codec Item () ()
bareItemEq = Lift . BareEq

item :: Iso BareItem a -> Codec Parameters p p -> Codec Item (a, p) (a, p)
item vIso c = (,) <$> (fst =. Lift (Bare vIso)) <*> (snd =. Lift (Params c))

params :: Codec Parameters p p -> Codec Item p p
params = Lift . Params

raw :: Codec Item ByteString ByteString
raw = Lift Raw

parseItem :: Codec Item i o -> ByteString -> (Either ParseError o, ByteString)
parseItem = Codec.parser alg
 where
  alg :: forall a. Item a -> ByteString -> (Either ParseError a, ByteString)
  alg t s = case t of
    Bare vIso -> (vIso.decode (strip bare), s)
    BareEq c  -> (if strip bare == c then Right () else Left ParseError, s)
    Params c  -> (fst (parseParameters c ps), s)
    Raw       -> (Right s, s)
   where
    (bare, ps) = firstAndRest 59 s

printItem :: Codec Item i o -> i -> ByteString
printItem = Codec.printer pr
 where
  pr :: forall a. Item a -> a -> ByteString
  pr (Bare vIso) v = vIso.encode v
  pr (BareEq c)  () = c
  pr (Params c)  p = printParameters c p
  pr Raw         bs = bs
