{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Okapi.Controller where

import qualified Control.Monad.Par as Par
-- import Control.Natural ((~>))

import Control.Natural
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as Text
import qualified GHC.Generics as Generics
import qualified Network.HTTP.Types as HTTP
import qualified Network.Wai as WAI
import qualified Okapi.Endpoint as Endpoint
import qualified Okapi.Endpoint.Body as Body
import qualified Okapi.Endpoint.Headers as Headers
import qualified Okapi.Endpoint.Path as Path
import qualified Okapi.Endpoint.Query as Query
import qualified Okapi.Endpoint.Responder as Responder
import qualified Okapi.Endpoint.ResponderHeaders as ResponderHeaders
import qualified Okapi.Params as Params

data Plan m p q h b r = Plan
  { -- | Handles side effects in the IO context.
    -- TODO: Use natural transformation.
    lifter :: m ~> IO,
    endpoint :: Endpoint.Endpoint p q h b r,
    handler :: Monad m => p -> q -> h -> b -> r %1 -> m ResponderHeaders.Response
  }

data Controller = Run (IO WAI.Response) | NoRun

make ::
  forall m p q h b r.
  ( Monad m,
    Par.NFData p,
    Par.NFData q,
    Par.NFData h,
    Par.NFData b,
    Par.NFData r
  ) =>
  Plan
    m
    p
    q
    h
    b
    r ->
  ( HTTP.StdMethod,
    [Text.Text],
    HTTP.Query,
    HTTP.RequestHeaders,
    LBS.ByteString
  ) ->
  Controller
make plan (method, path, query, headers, body) =
  if method == plan.endpoint.method
    then case Par.runPar $ parEval (endpoint plan) (path, query, headers, body) of
      (Right path, Right query, Right headers, Right body, Right responder) ->
        Run $ okapiResponseToWAIResponse <$> lifter plan (handler plan path query headers body responder)
      _ -> NoRun
    else NoRun

okapiResponseToWAIResponse :: ResponderHeaders.Response -> WAI.Response
okapiResponseToWAIResponse = undefined

parEval ::
  forall p q h b r.
  ( Par.NFData p,
    Par.NFData q,
    Par.NFData h,
    Par.NFData b,
    Par.NFData r
  ) =>
  Endpoint.Endpoint
    p
    q
    h
    b
    r ->
  ([Text.Text], HTTP.Query, HTTP.RequestHeaders, LBS.ByteString) ->
  Par.Par
    (Either Path.Error p, Either Query.Error q, Either Headers.Error h, Either Body.Error b, Either Responder.Error r)
parEval endpoint (pathInput, queryInput, headersInput, bodyInput) = do
  pathComp <- Par.spawnP (Path.eval endpoint.path pathInput)
  queryComp <- Par.spawnP (Query.eval endpoint.query queryInput)
  headersComp <- Par.spawnP (Headers.eval endpoint.headers headersInput)
  bodyComp <- Par.spawnP (Body.eval endpoint.body bodyInput)
  responderComp <- Par.spawnP (Responder.eval endpoint.responder ())
  (pathRes, _) <- Par.get pathComp
  (queryRes, _) <- Par.get queryComp
  (headersRes, _) <- Par.get headersComp
  (bodyRes, _) <- Par.get bodyComp
  (responderRes, _) <- Par.get responderComp
  return (pathRes, queryRes, headersRes, bodyRes, responderRes)
