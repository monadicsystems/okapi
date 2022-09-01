module Main where

import qualified Okapi

main :: IO ()
main = Okapi.run id Okapi.static
