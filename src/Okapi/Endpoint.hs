module Okapi.Endpoint where

import qualified Network.HTTP.Types as HTTP
import qualified Okapi.Endpoint.Body as Body
import qualified Okapi.Endpoint.Headers as Headers
import qualified Okapi.Endpoint.Path as Path
import qualified Okapi.Endpoint.Query as Query
import qualified Okapi.Endpoint.Responder as Responder

data Endpoint p q h b r = Endpoint
  { method :: HTTP.Method,
    path :: Path.Path p,
    query :: Query.Query q,
    headers :: Headers.Headers h,
    body :: Body.Body b,
    responder :: Responder.Responder r
  }