{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Okapi.Request.Vault
  ( Parser (..),
    Vault.Vault,
    parse,
    insert,
    delete,
    adjust,
    wipe,
    Okapi.Request.Vault.lookup,
  )
where

import qualified Control.Monad as Monad
import qualified Control.Monad.Except as Except
import qualified Control.Monad.Logger as Logger
import qualified Data.Vault.Lazy as Vault
import qualified Okapi.Error as Error
import qualified Okapi.Internal.Error as Error
import Okapi.Internal.Request.Vault

type Parser m = (Monad.MonadPlus m, Except.MonadError Error.Error m, Logger.MonadLogger m, State m)

-- TODO: Fail if Vault is empty
parse :: Parser m => m Vault.Vault
parse = get

insert :: State m => Vault.Key a -> a -> m ()
insert key value = do
  vault <- get
  modify $ Vault.insert key value

delete :: State m => Vault.Key a -> m ()
delete key = do
  vault <- get
  modify $ Vault.delete key

adjust :: State m => (a -> a) -> Vault.Key a -> m ()
adjust adjuster key = do
  vault <- get
  modify $ Vault.adjust adjuster key

wipe :: State m => m ()
wipe = put Vault.empty

-- $vaultParsers

lookup :: Parser m => Vault.Key a -> m a
lookup key = do
  vault <- get
  maybe Error.next pure (Vault.lookup key vault)
