module Okapi.State.Request.Vault where

import qualified Data.Vault.Lazy as Vault
import qualified Okapi.Type.Request as Request

class Monad m => MonadState m where
  get :: m Vault.Vault
  put :: Vault.Vault -> m ()

gets :: MonadState m => (Vault.Vault -> a) -> m a
gets projection = projection <$> get

modify :: MonadState m => (Vault.Vault -> Vault.Vault) -> m ()
modify modifier = do
  request <- get
  put $ modifier request
