{-# LANGUAGE ImportQualifiedPost #-}

module Okapi.Middleware where

import Network.Wai qualified as Wai

class To a where
  to :: a -> Wai.Middleware
