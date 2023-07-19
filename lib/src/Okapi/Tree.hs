module Okapi.Tree where

data Tree a = Nil | Leaf a | (Tree a) :|: (Tree a)

