{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Okapi.Body where

import Data.Text qualified as Text
import Data.Typeable qualified as Typeable
import Text.Regex.TDFA qualified as Regex
import Web.HttpApiData qualified as Web

data Parser a where
    FMap :: (a -> b) -> Parser a -> Parser b
    Pure :: a -> Parser a
    Apply :: Parser (a -> b) -> Parser a -> Parser b
    Match :: forall a. (Web.ToHttpApiData a) => a -> Parser ()
    Param :: forall a. (Typeable.Typeable a, Web.FromHttpApiData a) => Parser a
    Regex :: forall a. (Regex.RegexContext Regex.Regex Text.Text a) => Text.Text -> Parser a

class From a where
    parser :: Parser a
    parse :: ()
