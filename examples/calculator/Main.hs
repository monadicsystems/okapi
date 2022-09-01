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
  segMatch @Text "calc"
  addOp <|> subOp <|> mulOp <|> divOp

respond :: Response -> Okapi Response
respond response = do
  methodEnd
  pathEnd
  pure response

addOp :: Okapi Response
addOp = do
  segMatch @Text "add"
  (x, y) <- getArgs
  ok
    & setJSON (x + y)
    & respond

subOp :: Okapi Response
subOp = do
  segMatch @Text "sub" <|> segMatch @Text "minus"
  (x, y) <- getArgs
  ok
    & setJSON (x - y)
    & respond

mulOp :: Okapi Response
mulOp = do
  segMatch @Text "mul"
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
  segMatch @Text "div"
  (x, y) <- getArgs
  if y == 0
    then throw $ Response 403 [] $ ResponseBodyRaw "Forbidden"
    else
      ok
        & setJSON DivResult {answer = x `div` y, remainder = x `mod` y}
        & respond

getArgs :: Okapi (Int, Int)
getArgs = getArgsFromPath <|> getArgsFromQueryParams
  where
    getArgsFromPath :: Okapi (Int, Int)
    getArgsFromPath = do
      x <- seg
      y <- seg
      pure (x, y)

    getArgsFromQueryParams :: Okapi (Int, Int)
    getArgsFromQueryParams = do
      x <- queryParam "x"
      y <- queryParam "y"
      pure (x, y)
