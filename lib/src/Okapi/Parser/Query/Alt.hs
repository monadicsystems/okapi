{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Okapi.Parser.Query.Alt where

import Data.Bifunctor qualified as Bifunctor
import Data.ByteString qualified as BS
import Data.List qualified as List
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Network.HTTP.Types qualified as HTTP
import Web.HttpApiData qualified as Web
import Network.Wai qualified as Wai
import Network.Wai.Internal qualified as Wai
import Okapi.Parser.Query.Operation qualified as Operation
import Control.Applicative (Alternative(..))
import Okapi.Tree qualified as Tree

data Parser a where
  FMap :: (a -> b) -> Parser a -> Parser b
  Pure :: a -> Parser a
  Apply :: Parser (a -> b) -> Parser a -> Parser b
  Empty :: Parser a
  Or :: Parser a -> Parser a -> Parser a
  Optional :: Parser a -> Parser (Maybe a)
  Option :: a -> Parser a -> Parser a
  Operation :: Operation.Parser a -> Parser a

instance Functor Parser where
  fmap = FMap

instance Applicative Parser where
  pure = Pure
  (<*>) = Apply

instance Alternative Parser where
  empty = Empty
  (<|>) = Or

param :: Web.FromHttpApiData a => BS.ByteString -> Parser a
param = Operation . Operation.Param

flag :: BS.ByteString -> Parser ()
flag = Operation . Operation.Flag

optional :: Web.FromHttpApiData a => Parser a -> Parser (Maybe a)
optional = Optional

option :: Web.FromHttpApiData a => a -> Parser a -> Parser a
option = Option

eval ::
  Parser a ->
  Wai.Request ->
  (Either (Tree.Tree Operation.Error) a, Wai.Request)
eval (FMap f opX) state = case eval opX state of
    (Left e, state') -> (Left e, state')
    (Right x, state') -> (Right $ f x, state')
eval (Pure x) state = (Right x, state)
eval (Apply opF opX) state = case eval opF state of
    (Right f, state') -> case eval opX state' of
      (Right x, state'') -> (Right $ f x, state'')
      (Left e, state'') -> (Left e, state'')
    (Left e, state') -> (Left e, state')
eval Empty state = (Left Tree.Nil, state)
eval (Or opA opB) state = case eval opA state of
    (Right a, state') -> (Right a, state')
    (Left l, state') -> case eval opB state' of
        (Right b, state'') -> (Right b, state'')
        (Left r, state'') -> (Left (l Tree.:|: r), state'')
eval (Optional op) state = case op of
    Operation param@(Operation.Param _) -> case Operation.eval param state of
      (Right result, state') -> (Right $ Just result, state')
      (_, state') -> (Right Nothing, state')
    Operation flag@(Operation.Flag _) -> case Operation.eval flag state of
      (Right result, state') -> (Right $ Just result, state')
      (_, state') -> (Right Nothing, state')
    _ -> case eval op state of
      (Right result, state') -> (Right $ Just result, state')
      (Left err, state') -> (Left err, state')
eval (Option def op) state = case op of
    Operation param@(Operation.Param _) -> case Operation.eval param state of
      (Right result, state') -> (Right result, state')
      (_, state') -> (Right def, state')
    Operation flag@(Operation.Flag _) -> case Operation.eval flag state of
      (Right result, state') -> (Right result, state')
      (_, state') -> (Right def, state')
    _ -> eval op state
eval (Operation op) state = Bifunctor.first (Bifunctor.first Tree.Leaf) $ Operation.eval op state

class FromQuery a where
  parser :: Parser a

parse :: FromQuery a => Wai.Request -> Either (Tree.Tree Operation.Error) a
parse req = fst $ eval parser req
