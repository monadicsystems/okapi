{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Okapi.Request.Vault
  ( module Vault,
    MonadVault (..),
    State (..),
    gets,
    modify,
    look,
    vaultInsert,
    vaultDelete,
    vaultAdjust,
    vaultWipe,
    vaultLookup,
  )
where

import qualified Control.Monad as Monad
import qualified Control.Monad.Except as Except
import qualified Control.Monad.Logger as Logger
import qualified Data.Vault.Lazy as Vault
import qualified Okapi.Error as Error

type MonadVault m = (Monad.MonadPlus m, Except.MonadError Error.Error m, Logger.MonadLogger m, State m)

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

vaultInsert :: State m => Vault.Key a -> a -> m ()
vaultInsert key value = do
  vault <- get
  modify $ Vault.insert key value

vaultDelete :: State m => Vault.Key a -> m ()
vaultDelete key = do
  vault <- get
  modify $ Vault.delete key

vaultAdjust :: State m => (a -> a) -> Vault.Key a -> m ()
vaultAdjust adjuster key = do
  vault <- get
  modify $ Vault.adjust adjuster key

vaultWipe :: State m => m ()
vaultWipe = put Vault.empty

-- $vaultParsers

vaultLookup :: MonadVault m => Vault.Key a -> m a
vaultLookup key = do
  vault <- get
  maybe Error.next pure (Vault.lookup key vault)
