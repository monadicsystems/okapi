{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module Okapi.Route.Pattern where

import Data.Text
import Data.Typeable
import Web.HttpApiData qualified as Web

pattern Part :: forall a. (Web.FromHttpApiData a, Web.ToHttpApiData a) => a -> Text
pattern Part x <- (Web.parseUrlPiece -> Right x)
  where
    Part x = Web.toUrlPiece x
