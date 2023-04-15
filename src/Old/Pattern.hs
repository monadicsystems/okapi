{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Okapi.Pattern where

import qualified Web.HttpApiData as Web
import qualified Data.Text as Text
import qualified Network.HTTP.Types as HTTP

pattern GET :: HTTP.Method
pattern GET = "GET"

pattern POST :: HTTP.Method
pattern POST = "POST"

pattern DELETE :: HTTP.Method
pattern DELETE = "DELETE"

pattern OTHER :: HTTP.Method -> HTTP.Method
pattern OTHER method <-
  method
  where
    OTHER method = method

pattern PathParam :: (Web.ToHttpApiData a, Web.FromHttpApiData a) => a -> Text.Text
pattern PathParam param <-
  (Web.parseUrlPiece -> Right param)
  where
    PathParam param = Web.toUrlPiece param


