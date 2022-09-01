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

type Okapi a = Okapi.OkapiT IO a

data DivResult = DivResult
  { answer :: Int,
    remainder :: Int
  }
  deriving (Eq, Show, Generics.Generic, Aeson.ToJSON)

pattern PathParams :: Int -> Int -> Okapi.Path
pattern PathParams x y = [Okapi.Seg x, Okapi.Seg y]

pattern OpNoPathParams :: Text.Text -> Okapi.Path
pattern OpNoPathParams op = ["calc", op]

pattern OpHasPathParams :: Text.Text -> Int -> Int -> Okapi.Path
pattern OpHasPathParams op x y = ["calc", op, Okapi.Seg x, Okapi.Seg y]

main :: IO ()
main = Okapi.run id $ do
  Okapi.route $ \case
    OpNoPathParams op -> do
      (x, y) <- getArgsFromQuery
      handle op x y
    OpHasPathParams op x y -> handle op x y
    _ -> Okapi.next

getArgsFromQuery :: Okapi (Int, Int)
getArgsFromQuery = do
  x <- Okapi.queryParam "x"
  y <- Okapi.queryParam "y"
  pure (x, y)

handle :: Text.Text -> Int -> Int -> Okapi Okapi.Response
handle op x y = case op of
  "add" -> Okapi.ok Function.& Okapi.setJSON (x + y) Function.& respond
  "sub" -> Okapi.ok Function.& Okapi.setJSON (x - y) Function.& respond
  "minus" -> Okapi.ok Function.& Okapi.setJSON (x - y) Function.& respond
  "mul" -> Okapi.ok Function.& Okapi.setJSON (x * y) Function.& respond
  "div" -> do
    Okapi.guardThrow Okapi.forbidden (y == 0)
    Okapi.ok Function.& Okapi.setJSON DivResult {answer = x `div` y, remainder = x `mod` y} Function.& respond

respond :: Okapi.Response -> Okapi Okapi.Response
respond response = do
  Okapi.methodEnd
  Okapi.pathEnd
  Okapi.queryEnd
  pure response
