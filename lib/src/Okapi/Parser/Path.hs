{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Okapi.Parser.Path where

import Data.Text
import Okapi.NewDSL
import qualified Web.HttpApiData as Web

data Expr a where
  Static :: Web.ToHttpApiData a => a -> Expr ()
  Param :: Web.FromHttpApiData a => Expr a
  End :: Expr ()

type State = [Text]

data Error where
  Error :: Text -> Error

interpreter :: Interpreter Expr State Error a
interpreter state expr = case expr of
  Static @t x -> undefined
  Param @t -> undefined
  End -> undefined

embed :: Expr a -> DSL Expr State Error a
embed = Eval interpreter

static :: Web.ToHttpApiData a => a -> DSL Expr State Error ()
static x = embed $ Static x

param :: Web.FromHttpApiData a => DSL Expr State Error a
param = embed Param

end :: Web.FromHttpApiData a => DSL Expr State Error ()
end = embed End

-- instance DSL Expr [Text] Error where
--   eval :: Expr -> [Text] -> (Either Error Result, [Text])
--   eval (Static @t x) input = (Right $ StaticResult (), [])
--   eval (Param @t) input = undefined
--   eval End [] = (Right $ EndResult (), [])
