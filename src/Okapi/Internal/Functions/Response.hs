{-# LANGUAGE RankNTypes #-}
module Okapi.Internal.Response where

import Okapi.Internal.Types

-- RESPONSE SETTERS (Hidden)

setResponseStatus :: Natural.Natural -> Response -> Response
setResponseStatus status response = response {responseStatus = status}

setResponseHeaders :: Headers -> Response -> Response
setResponseHeaders headers response = response {responseHeaders = headers}

-- TODO: setResponseCookie

setResponseHeader :: HTTP.Header -> Response -> Response
setResponseHeader header response@Response {..} =
  response {responseHeaders = update header responseHeaders}
  where
    update :: forall a b. Eq a => (a, b) -> [(a, b)] -> [(a, b)]
    update pair [] = [pair]
    update pair@(key, value) (pair'@(key', value') : ps) =
      if key == key'
        then pair : ps
        else pair' : update pair ps

setResponseBody :: ResponseBody -> Response -> Response
setResponseBody body response = response {responseBody = body}
