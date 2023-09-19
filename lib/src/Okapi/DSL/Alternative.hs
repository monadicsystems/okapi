{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE GADTs #-}

module Okapi.DSL.Alternative where

import Okapi.Tree
import Okapi.DSL

-- 3 possibilities:
-- 1. No errors. Just output. Means nothing went wrong.
-- 2. Some errors. Just output. Means something went wrong, but program was able to recover.
-- 3. Errors. Nothing output. Means everything went wrong.

exec :: DSL expr input error => Program expr input (Tree error) (Maybe output) -> input -> (Maybe output, (input, Tree error))
exec (FMap f prog) input = case exec prog input of
  (Nothing, (input', eTree)) -> (Nothing, (input', eTree))
  (Just o, (input', eTree)) -> (Just o, (input', eTree))
exec (Pure x) input = (Just x, (input, Nil))
exec (Apply progF progX) input = case exec progF input of
  (Just f, (input', eTreeF)) -> case exec progX input of
    (Just x, (input'', eTreeX)) -> (Just $ f x, (input'', eTreeF :->: eTreeX ))
    (Nothing, (input'', eTreeX)) -> (Nothing, (input'', eTreeF :->: eTreeX))
  (Nothing, (input', eTreeF)) -> (Nothing, (input', eTreeF))
eval Empty input = (Nothing, (input, Nil))
eval (Or progA progB) input = case exec progA input of
  (Just a, (input', eTreeA)) -> (Just a, (input', eTreeA))
  (Nothing, (_, eTreeA)) -> case exec progB input of
    (Just b, (input', eTreeB)) -> (Just b, (input', eTreeA :<|>: eTreeB))
    (Nothing, (input', eTreeB)) -> (Nothing, (input', eTreeA :<|>: eTreeB))
exec (Expr expr) input = case eval expr input of
    (Left error, input') -> (Nothing, (input', Leaf error))
    (Right x, input') -> (Just x, (input', Nil))

data Program expr input error output where
  FMap :: (output -> output') -> Program expr input error output -> Program expr input error output'
  Pure :: output -> Program expr input error output
  Apply :: Program expr input error (output -> output') -> Program expr input error output -> Program expr input error output'
  Empty :: Program expr input error output
  Or :: Program expr input error output -> Program expr input error output -> Program expr input error output
  Expr :: expr -> Program expr input error output

instance Functor (Program expr input error) where
  fmap = FMap

instance Applicative (Program expr input error) where
  pure = Pure
  (<*>) = Apply

instance Alternative (Program expr input error) where
  empty = Empty
  (<|>) = Or
