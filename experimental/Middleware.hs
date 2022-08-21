{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Okapi.Middleware where

import Data.Text
import Okapi.Parser
import Okapi.Response
import Okapi.Types

applyMiddlewares :: MonadOkapi m => [m Response -> m Response] -> m Response -> m Response
applyMiddlewares ms handler =
  Prelude.foldl (\handler m -> m handler) handler ms

addHeadersMiddleware :: MonadOkapi m => m Response -> m Response
addHeadersMiddleware handler = setHeaders [] <$> handler

prefixPathMiddeware :: MonadOkapi m => [Text] -> (m Response -> m Response)
prefixPathMiddeware prefix handler = path prefix >> handler

-- | TODO: Is this needed? Idea taken from OCaml Dream framework
scope :: MonadOkapi m => [Text] -> [m Response -> m Response] -> (m Response -> m Response)
scope prefix middlewares handler = do
  path prefix
  applyMiddlewares middlewares handler
