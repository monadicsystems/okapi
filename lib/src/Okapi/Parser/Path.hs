{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Okapi.Parser.Path where

import Data.Text
import Okapi.DSL
import qualified Web.HttpApiData as Web

data Expr where
  Static :: Web.ToHttpApiData a => a -> Expr
  Param :: Web.FromHttpApiData a => Expr
  End :: Expr

data Error where
  Error :: Text -> Error

data Result where
  StaticResult :: () -> Result
  ParamResult :: Web.FromHttpApiData a => a -> Result
  EndResult :: () -> Result
  

instance DSL Expr [Text] Error where
  eval :: Expr -> [Text] -> (Either Error Result, [Text])
  eval (Static @t x) input = (Right $ StaticResult (), [])
  eval (Param @t) input = undefined
  eval End [] = (Right $ EndResult (), [])
