{-# LANGUAGE NoFieldSelectors #-}

module Okapi.Response.Result (Response (..)) where

import Okapi.HTTP.Tree (Failure, ForResponse)
import Okapi.HTTP.Headers (Headers)
import Okapi.HTTP.Response.Status qualified as Status

data Response status headers body = Response
    { status  :: Either Status.ParseError status
    , headers :: Either (Failure (Headers ForResponse)) headers
    , body    :: body
    }
