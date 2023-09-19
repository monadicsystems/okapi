{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE GADTs #-}

module Okapi.DSL.Applicative where

import Okapi.DSL

-- data Program expr input error output where
--   FMap :: (output -> output') -> Program expr input error output -> Program expr input error output'
--   Pure :: output -> Program expr input error output
--   Apply :: Program expr input error (output -> output') -> Program expr input error output -> Program expr input error output'
--   Expr :: expr -> Program expr input error output

-- exec :: forall expr input error output. DSL expr input error output => Program expr input error output -> input -> (Either error output, input)
-- exec (FMap f prog) input = case exec @expr @input @error @output prog input of
--   (Left e, input') -> (Left e, input')
--   (Right o, input') -> (Right $ f o, input')
-- exec (Pure x) input = (Right x, input)
-- exec (Apply progF progX) input = case exec progF input of
--   (Right f, input') -> case exec progX input' of
--     (Right x, input'') -> (Right $ f x, input'')
--     (Left e, input'') -> (Left e, input'')
--   (Left e, input') -> (Left e, input')
-- exec (Expr expr) input = eval expr input

-- instance Functor (Program expr input error) where
--   fmap = FMap

-- instance Applicative (Program expr input error) where
--   pure = Pure
--   (<*>) = Apply
