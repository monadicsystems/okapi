{-# LANGUAGE GADTs #-}

module Okapi.Parser.Body.JSON where

import qualified Data.Aeson as Aeson

data JSON a where
  JSON :: Aeson.FromJSON a => JSON a