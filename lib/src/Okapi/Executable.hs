{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Okapi.Executable where

import Control.Monad.Par qualified as Par
-- import Control.Natural ((~>))

import Control.Natural
import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as Text
import GHC.Generics qualified as Generics
import Network.HTTP.Types qualified as HTTP
import Network.Wai qualified as WAI
import Okapi.Endpoint (Endpoint)
import Okapi.Endpoint qualified as Endpoint
import Okapi.Request
import Okapi.Response (Response)
import Okapi.Script
import Okapi.Script.Body qualified as Body
import Okapi.Script.Headers qualified as Headers
import Okapi.Script.Path qualified as Path
import Okapi.Script.Query qualified as Query
import Okapi.Script.Responder qualified as Responder
import Okapi.Script.ResponderHeaders qualified as ResponderHeaders

data Plan m p q h b r = Plan
  { transformer :: m ~> IO,
    endpoint :: Endpoint p q h b r,
    handler :: Monad m => p -> q -> b -> h -> r -> m Response
  }

data Executable = Run (IO WAI.Response) | Null

type Compiler = Request -> Executable

executable ::
  forall m p q h b r.
  Monad m =>
  Plan m p q h b r ->
  Compiler
executable plan (method, path, query, body, headers) =
  if method == plan.endpoint.method
    then
      let pathResult = fst $ Path.eval plan.endpoint.pathScript path
          queryResult = fst $ Query.eval plan.endpoint.queryScript query
          bodyResult = fst $ Body.eval plan.endpoint.bodyScript body
          headersResult = fst $ Headers.eval plan.endpoint.headersScript headers
          responderResult = fst $ Responder.eval plan.endpoint.responderScript ()
       in case (pathResult, queryResult, bodyResult, headersResult, responderResult) of
            (Ok p, Ok q, Ok b, Ok h, Ok r) -> Run do
              response <- transformer plan $ handler plan p q b h r
              return $ toWAIResponse response
            _ -> Null
    else Null

toWAIResponse :: Response -> WAI.Response
toWAIResponse = undefined
