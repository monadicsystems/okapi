{-# LANGUAGE NoFieldSelectors #-}

module Okapi.Failure.Request (Request (..)) where

import Okapi.HTTP.Tree (Failure)
import Okapi.HTTP.Request.Method qualified as Method
import Okapi.HTTP.Request.Path (Path)
import Okapi.HTTP.Request.Query (Query)
import Okapi.HTTP.Request.Headers (RequestHeaders)
import Okapi.HTTP.Request.Body qualified as ReqBody

data Request method path query headers body = Request
    { method  :: Maybe Method.ParseError
    , path    :: Maybe (Failure Path)
    , query   :: Maybe (Failure Query)
    , headers :: Maybe (Failure RequestHeaders)
    , body    :: Maybe ReqBody.ParseError
    }
