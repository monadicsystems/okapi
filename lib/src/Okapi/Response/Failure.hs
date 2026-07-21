{-# LANGUAGE NoFieldSelectors #-}

module Okapi.Response.Failure (Response (..)) where

import Okapi.HTTP.Tree (Failure, ForResponse)
import Okapi.HTTP.Headers (Headers)
import Okapi.HTTP.Response.Status qualified as Status

data Response status headers body = Response
    { status  :: Maybe Status.ParseError
    , headers :: Maybe (Failure (Headers ForResponse))
    }
