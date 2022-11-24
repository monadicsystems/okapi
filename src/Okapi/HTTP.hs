{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Okapi.HTTP
  ( Parser (..)
  )
where

import qualified Control.Monad as Monad
import qualified Control.Monad.Except as Except
import qualified Control.Monad.Logger as Logger
import qualified Okapi.Internal.Error as Error
import Okapi.Internal.HTTP
import qualified Okapi.Internal.Request
import qualified Okapi.Internal.Request as Request
import qualified Okapi.Internal.Response
import qualified Okapi.Internal.Response as Response

type Parser m = (Except.MonadError Error.Error m, Logger.MonadLogger m, Monad.MonadPlus m, State m)
