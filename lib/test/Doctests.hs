module Main where

import Test.DocTest (mainFromCabal)

main :: IO ()
main = mainFromCabal "okapi" []
