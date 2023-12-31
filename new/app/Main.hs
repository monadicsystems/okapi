module Main where

import qualified Okapi (someFunc)

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  Okapi.someFunc
