{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Data.Text
import Okapi
import Rel8

main :: IO ()
main = run id $ do
  methodGET
  pathParam @Text `is` "hello"
  return $ setHTML "<h1>Hello world</h1>" $ ok
