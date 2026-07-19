{-# LANGUAGE NoFieldSelectors #-}

module Okapi.Failure.Response (Response (..)) where

import Okapi.HTTP.Tree (Failure)
import Okapi.HTTP.Response.Status qualified as Status
import Okapi.HTTP.Response.Headers (ResponseHeaders)
import Okapi.HTTP.Response.Body qualified as ResBody

data Response status headers body = Response
    { status  :: Maybe Status.ParseError
    , headers :: Maybe (Failure ResponseHeaders)
    , body    :: Maybe ResBody.ParseError
    }
