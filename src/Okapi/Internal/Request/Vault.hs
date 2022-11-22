{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Okapi.Internal.Request.Vault
  ( State (..),
    gets,
    modify,
    look,
  )
where

import qualified Control.Monad as Monad
import qualified Control.Monad.Except as Except
import qualified Control.Monad.Logger as Logger
import qualified Data.Vault.Lazy as Vault
import qualified Okapi.Internal.Error as Error

class Monad m => State m where
  get :: m Vault.Vault
  put :: Vault.Vault -> m ()

gets :: State m => (Vault.Vault -> a) -> m a
gets projection = projection <$> get

modify :: State m => (Vault.Vault -> Vault.Vault) -> m ()
modify modifier = do
  request <- get
  put $ modifier request

look :: State m => m a -> m a
look action = do
  request <- get
  result <- action
  put request
  pure result
