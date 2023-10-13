{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Okapi.NewDSL where
import Data.Kind (Type)
import Data.Typeable

-- type Interpreter (expr :: * -> *) state error a = state -> expr a -> (Either error a, state)

class Context (expr :: * -> *) state error where
  eval :: state -> expr a -> (Either error a, state)

data DSL (expr :: * -> *) state error a where
  FMap :: (a -> b) -> DSL expr state error a -> DSL expr state error b
  Pure :: a -> DSL expr state error a
  Apply :: DSL expr state error (a -> b) -> DSL expr state error a -> DSL expr state error b
--   Eval :: Interpreter expr state error a -> expr a -> DSL expr state error a
  Expr :: Context expr state error => expr a -> DSL expr state error a -- Call Quote?
  -- Add constructor for Combinator??
  -- deriving (Typeable)

instance Functor (DSL expr state error) where
  fmap = FMap

instance Applicative (DSL expr state error) where
  pure = Pure
  (<*>) = Apply

exec :: Context expr state error => state -> DSL expr state error a -> (Either error a, state)
exec state (FMap f expr) = case exec state expr of
  (Left e, state') -> (Left e, state')
  (Right o, state') -> (Right $ f o, state')
exec state (Pure x) = (Right x, state)
exec state (Apply progF progX) = case exec state progF of
  (Right f, state') -> case exec state' progX of
    (Right x, state'') -> (Right $ f x, state'')
    (Left e, state'') -> (Left e, state'')
  (Left e, state') -> (Left e, state')
exec state (Expr expr) = eval state expr

-- expr :: Context expr state error => expr a -> DSL expr state error a
-- expr = Expr
