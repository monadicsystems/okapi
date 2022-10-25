module Okapi.Middleware where

import qualified Data.List as List
import qualified Okapi.Request as Request
import qualified Okapi.Response as Response
import qualified Okapi.Server as Server
import qualified Okapi.Server.Failure as Failure
import qualified Okapi.Server.Request as Request hiding (path)
import qualified Okapi.Server.Response as Response

-- $middleware
--
-- Middlewares allow you to modify the behavior of Okapi handlers.
-- Middlewares are functions that take a handler and return another handler.
-- Middlewares can be composed with the fish operator @>=>@.
--
-- @
--  clearHeadersMiddleware >=> pathPrefix ["jello"] :: forall m. Middleware m
-- @

applyMiddlewares :: Server.ServerM m => [m () -> m ()] -> (m () -> m ())
applyMiddlewares middlewares handler =
  List.foldl (\handler middleware -> middleware handler) handler middlewares

-- TODO: Is this needed? Idea taken from OCaml Dream framework

scope :: Server.ServerM m => Request.Path -> [m () -> m ()] -> (m () -> m ())
scope prefix middlewares handler = Request.path `Failure.is` prefix >> applyMiddlewares middlewares handler

clearHeadersMiddleware :: Server.ServerM m => m () -> m ()
clearHeadersMiddleware handler = do
  Response.setHeaders []
  handler

prefixPathMiddleware :: Server.ServerM m => Request.Path -> (m () -> m ())
prefixPathMiddleware prefix handler = Request.path `Failure.is` prefix >> handler
