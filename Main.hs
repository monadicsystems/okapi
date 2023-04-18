{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Data.Text qualified as Text
import Okapi.Endpoint.Path qualified as Path
import Text.Pretty.Simple (pPrint)
import Web.HttpApiData qualified as Web

main :: IO ()
main = do
  print $ Path.eval path3 ["product", "cup", "99"]

newtype Category = Category {unwrap :: Text.Text}
  deriving newtype (Eq, Show, Web.FromHttpApiData)

newtype ProductID = ProductID {unwrap :: Int}
  deriving newtype (Eq, Show, Web.FromHttpApiData)

path3 :: Path.Path (Category, ProductID, Bool, Char)
path3 = Path.do
  category <- Path.param @Category "category"
  Path.static "blah"
  foo <- Path.param @Bool "foo"
  c <- Path.param @Char "c"
  Path.static "product"
  productID <- Path.param @ProductID "productID"
  Path.pure (category, productID, foo, c)
