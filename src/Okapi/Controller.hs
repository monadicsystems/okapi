{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

module Okapi.Controller where

import qualified Network.HTTP.Types as HTTP
import qualified Okapi.Endpoint as Endpoint
import qualified Okapi.Endpoint.Body as Body
import qualified Okapi.Endpoint.Headers as Headers
import qualified Okapi.Endpoint.Path as Path
import qualified Okapi.Endpoint.Query as Query
import qualified Okapi.Endpoint.Responder as Responder
import qualified Okapi.Params as Params
import qualified Okapi.Response as Response

data Plan m p q h b r = Plan
  { -- | Handles side effects in the IO context.
    -- TODO: Use natural transformation.
    lifter :: forall m a. Monad m => m a -> IO a,
    endpoint :: Endpoint.Endpoint p q h b r,
    handler :: Monad m => Params.Params p q h b r -> m Response.Response
  }

data Controller = Controller

controller ::
  forall m p q h b r.
  Monad m =>
  Plan m p q h b r ->
  Controller
controller = const Controller
