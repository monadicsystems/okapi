{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Okapi.Request
  ( Parser (..),
    Request,
    end,
    parse,
  )
where

import qualified Control.Monad as Monad
import qualified Control.Monad.Except as Except
import qualified Control.Monad.Logger as Logger
import qualified Okapi.Request.Body as Body
import qualified Okapi.Request.Headers as Headers
import qualified Okapi.Request.Method as Method
import qualified Okapi.Request.Path as Path
import qualified Okapi.Request.Query as Query
import qualified Okapi.Request.Vault as Vault
import qualified Okapi.Internal.Error as Error
import Okapi.Internal.Request
import qualified Okapi.Internal.Request.Body as Internal.Body
import qualified Okapi.Internal.Request.Headers as Internal.Headers
import qualified Okapi.Internal.Request.Method as Internal.Method
import qualified Okapi.Internal.Request.Path as Internal.Path
import qualified Okapi.Internal.Request.Query as Internal.Query
import qualified Okapi.Internal.Request.Vault as Internal.Vault

type Parser m = (Monad.MonadPlus m, Except.MonadError Error.Error m, Logger.MonadLogger m, State m)

parse :: Parser m => m Request
parse = Request <$> Method.parse <*> Path.parse <*> Query.parse <*> Body.parse <*> Headers.parse <*> Vault.parse

end :: Parser m => m ()
end = do
  Method.end
  Path.end
  Query.end
  Body.end
  Headers.end
