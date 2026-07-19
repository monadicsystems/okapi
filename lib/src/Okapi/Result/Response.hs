{-# LANGUAGE NoFieldSelectors #-}

module Okapi.Result.Response (Response (..)) where

import Okapi.HTTP.Tree (Failure)
import Okapi.HTTP.Response.Status qualified as Status
import Okapi.HTTP.Response.Headers (ResponseHeaders)
import Okapi.HTTP.Response.Body qualified as ResBody

data Response status headers body = Response
    { status  :: Either Status.ParseError status
    , headers :: Either (Failure ResponseHeaders) headers
    , body    :: Either ResBody.ParseError body
    }
