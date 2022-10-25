{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import qualified Control.Monad.Combinators as Combinator
import qualified Data.Aeson as Aeson
import qualified Data.Function as Function
import qualified Data.Text as Text
import qualified GHC.Generics as Generics
import qualified Okapi
import qualified Web.HttpApiData as Web

type Okapi a = Okapi.ServerT IO a

data DivResult = DivResult
  { answer :: Int,
    remainder :: Int
  }
  deriving (Eq, Show, Generics.Generic, Aeson.ToJSON)

data Op = AddOp | SubOp | MulOp | DivOp deriving (Eq, Show)

instance Web.FromHttpApiData Op where
  parseUrlPiece = \case
    "add" -> Right AddOp
    "sub" -> Right SubOp
    "minus" -> Right SubOp
    "mul" -> Right MulOp
    "div" -> Right DivOp
    _ -> Left "Couldn't parse operator from URL piece"

instance Web.ToHttpApiData Op where
  toUrlPiece = \case
    AddOp -> "add"
    SubOp -> "sub"
    MulOp -> "mul"
    DivOp -> "div"

pattern OpNoPathParams :: Op -> Okapi.Path
pattern OpNoPathParams op = ["calc", Okapi.PathParam op]

pattern OpHasPathParams :: Op -> Int -> Int -> Okapi.Path
pattern OpHasPathParams op x y = ["calc", Okapi.PathParam op, Okapi.PathParam x, Okapi.PathParam y]

main :: IO ()
main = Okapi.run id calculator

calculator :: Okapi Okapi.Effect.Response
calculator =
  Okapi.route Okapi.path $ \case
    OpNoPathParams op -> do
      x <- Okapi.queryParam "x"
      y <- Okapi.queryParam "y"
      handle op x y
    OpHasPathParams op x y -> handle op x y
    _ -> Okapi.next

handle :: Op -> Int -> Int -> Okapi Okapi.Effect.Response
handle op x y = case op of
  AddOp -> Okapi.ok Function.& Okapi.setJSON (x + y) Function.& respond
  SubOp -> Okapi.ok Function.& Okapi.setJSON (x - y) Function.& respond
  MulOp -> Okapi.ok Function.& Okapi.setJSON (x * y) Function.& respond
  DivOp -> do
    Okapi.guardThrow Okapi.forbidden (y == 0)
    Okapi.ok Function.& Okapi.setJSON DivResult {answer = x `div` y, remainder = x `mod` y} Function.& respond

respond :: Okapi.Effect.Response -> Okapi Okapi.Effect.Response
respond response = do
  Okapi.methodEnd
  Okapi.pathEnd
  Okapi.queryEnd
  pure response
