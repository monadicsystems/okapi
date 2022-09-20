{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Applicative ((<|>))
import Data.Aeson (ToJSON)
import Data.Function ((&))
import Data.Text
import GHC.Generics (Generic)
import Okapi

main :: IO ()
main = run id calc

type Okapi a = OkapiT IO a

calc :: Okapi Response
calc = do
  methodGET
  pathParam @Text `is` "calc"
  addOp <|> subOp <|> mulOp <|> divOp

respond :: Response -> Okapi Response
respond response = do
  methodEnd
  pathEnd
  queryEnd
  pure response

addOp :: Okapi Response
addOp = do
  pathParam @Text `is` "add"
  (x, y) <- getArgs
  ok
    & setJSON (x + y)
    & respond

subOp :: Okapi Response
subOp = do
  pathParam @Text `is` "sub" <|> pathParam @Text `is` "minus"
  (x, y) <- getArgs
  ok
    & setJSON (x - y)
    & respond

mulOp :: Okapi Response
mulOp = do
  pathParam @Text `is` "mul"
  (x, y) <- getArgs
  ok
    & setJSON (x * y)
    & respond

data DivResult = DivResult
  { answer :: Int,
    remainder :: Int
  }
  deriving (Eq, Show, Generic, ToJSON)

divOp :: Okapi Response
divOp = do
  pathParam @Text `is` "div"
  (x, y) <- getArgs
  guardThrow forbidden (y == 0)
  ok
    & setJSON DivResult {answer = x `div` y, remainder = x `mod` y}
    & respond

getArgs :: Okapi (Int, Int)
getArgs = getArgsFromPath <|> getArgsFromQueryParams
  where
    getArgsFromPath :: Okapi (Int, Int)
    getArgsFromPath = do
      x <- pathParam
      y <- pathParam
      pure (x, y)

    getArgsFromQueryParams :: Okapi (Int, Int)
    getArgsFromQueryParams = do
      x <- queryParam "x"
      y <- queryParam "y"
      pure (x, y)
