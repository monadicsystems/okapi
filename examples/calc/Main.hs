{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Applicative ((<|>))
import Data.Aeson (ToJSON)
import Data.Text
import GHC.Generics (Generic)
import Okapi


main :: IO ()
main = runOkapi id 3000 calc

type Okapi a = OkapiT IO a

calc :: Okapi Response
calc = do
  get
  seg "calc"
  addOp <|> subOp <|> mulOp <|> divOp

addOp :: Okapi Response
addOp = do
  seg "add"
  (x, y) <- getArgs
  respondJSON [] $ x + y

subOp :: Okapi Response
subOp = do
  seg "sub" <|> seg "minus"
  (x, y) <- getArgs
  respondJSON [] $ x - y

mulOp :: Okapi Response
mulOp = do
  seg "mul"
  (x, y) <- getArgs
  respondJSON [] $ x * y

data DivResult = DivResult
  { answer :: Int,
    remainder :: Int
  }
  deriving (Eq, Show, Generic, ToJSON)

divOp :: Okapi Response
divOp = do
  seg "div"
  (x, y) <- getArgs
  if y == 0
    then abort401 [] ""
    else respondJSON [] $ DivResult {answer = x `div` y, remainder = x `mod` y}

getArgs :: Okapi (Int, Int)
getArgs = getArgsFromPath <|> getArgsFromQueryParams
  where
    getArgsFromPath :: Okapi (Int, Int)
    getArgsFromPath = do
      x <- segParamAs @Int
      y <- segParamAs @Int
      pure (x, y)

    getArgsFromQueryParams :: Okapi (Int, Int)
    getArgsFromQueryParams = do
      x <- queryParamAs @Int "x"
      y <- queryParamAs @Int "y"
      pure (x, y)
