{-# LANGUAGE ImportQualifiedPost #-}

module Okapi.Middleware where

import Network.Wai qualified as Wai

class Tag a where
  fromTag :: a -> Wai.Middleware
