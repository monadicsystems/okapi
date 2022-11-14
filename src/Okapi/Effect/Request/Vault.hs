{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}

module Okapi.Effect.Request.Vault where

import qualified Control.Monad as Monad
import qualified Control.Monad.Except as Except
import qualified Control.Monad.Logger as Logger
import qualified Data.Vault.Lazy as Vault
import qualified Okapi.Effect.Failure as Failure
import qualified Okapi.State.Request.Vault as Vault
import qualified Okapi.Type.Failure as Failure

type MonadVault m = (Monad.MonadPlus m, Except.MonadError Failure.Failure m, Logger.MonadLogger m, Vault.MonadState m)

-- $vaultParsers

vault :: MonadVault m => m Vault.Vault
vault = Vault.get

vaultLookup :: MonadVault m => Vault.Key a -> m a
vaultLookup key = do
  vault <- Vault.get
  maybe Failure.next pure (Vault.lookup key vault)

vaultInsert :: MonadVault m => Vault.Key a -> a -> m ()
vaultInsert key value = do
  vault <- Vault.get
  Vault.modify $ Vault.insert key value

vaultDelete :: MonadVault m => Vault.Key a -> m ()
vaultDelete key = do
  vault <- Vault.get
  Vault.modify $ Vault.delete key

vaultAdjust :: MonadVault m => (a -> a) -> Vault.Key a -> m ()
vaultAdjust adjuster key = do
  vault <- Vault.get
  Vault.modify $ Vault.adjust adjuster key

vaultWipe :: MonadVault m => m ()
vaultWipe = Vault.put Vault.empty

look :: MonadVault m => m a -> m a
look action = do
  request <- Vault.get
  result <- action
  Vault.put request
  pure result
