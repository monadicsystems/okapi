{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}

module Okapi.Type.Failure where

import qualified Control.Monad.Except as Except
import qualified Okapi.Type.Response as Response

data Failure = Next | Abort Response.Response

instance Show Failure where
  show :: Failure -> String
  show Next = "Using next server"
  show (Abort _) = "Error returned"
