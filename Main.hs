module Main where

import Okapi.Scratch
import Text.Pretty.Simple (pPrint)

main :: IO ()
main = pPrint $ runXtractor myCounter $ 10