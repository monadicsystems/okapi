module Okapi.NewDSL where

data Error = Error

data Program a where
  FMap :: (a -> a') -> Program a -> Program a'
  Pure :: a -> Program a
  Apply :: Program (a -> a') -> Program a -> Program a'
  Expr :: Expr a -> Program a

instance Functor Program where
  fmap = FMap

instance Applicative Program where
  pure = Pure
  (<*>) = Apply
