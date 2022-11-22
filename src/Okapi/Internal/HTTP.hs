{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module Okapi.Internal.HTTP where

import qualified Control.Applicative as Applicative
import qualified Control.Monad as Monad
import qualified Control.Monad.Except as Except
import qualified Control.Monad.IO.Class as IO
import qualified Control.Monad.Logger as Logger
import qualified Control.Monad.Morph as Morph
import qualified Control.Monad.Reader.Class as Reader
import qualified Control.Monad.State.Strict as State
import qualified Data.Text as Text
import qualified Data.Vault.Lazy as Vault
import qualified Okapi.Internal.Error as Error
import qualified Okapi.Internal.Request as Request
import qualified Okapi.Internal.Request.Body as Body
import qualified Okapi.Internal.Request.Headers as Headers
import qualified Okapi.Internal.Request.Method as Method
import qualified Okapi.Internal.Request.Path as Path
import qualified Okapi.Internal.Request.Query as Query
import qualified Okapi.Internal.Request.Vault as Vault
import qualified Okapi.Internal.Response as Response

class (Request.State m, Response.State m) => State m where
  get :: m (Request.Request, Response.Response)
  default get :: m (Request.Request, Response.Response)
  get = (,) <$> Request.get <*> Response.get
  put :: (Request.Request, Response.Response) -> m ()
  default put :: (Request.Request, Response.Response) -> m ()
  put (request, response) = Request.put request >> Response.put response

look :: State m => m a -> m a
look action = do
  requestAndResponse <- get
  result <- action
  put requestAndResponse
  pure result
