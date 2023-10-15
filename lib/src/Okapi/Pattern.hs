{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

module Okapi.Pattern where

import Data.Text
import Data.Typeable
import Web.HttpApiData qualified as Web

pattern Literal :: forall a. (Web.FromHttpApiData a, Web.ToHttpApiData a) => a -> Text
pattern Literal x <- (Web.parseUrlPiece -> Right x)
  where
    Literal x = Web.toUrlPiece x
