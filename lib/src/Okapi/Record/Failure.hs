{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}

module Okapi.Record.Failure where

import Okapi.Tree (Failure)
import Okapi.HTTP.Request.Method qualified as Method
import Okapi.HTTP.Request.Path (Path)
import Okapi.HTTP.Request.Query (Query)
import Okapi.HTTP.Request.Headers (RequestHeaders)
import Okapi.HTTP.Request.Body qualified as ReqBody
import Okapi.HTTP.Response.Status qualified as Status
import Okapi.HTTP.Response.Headers (ResponseHeaders)
import Okapi.HTTP.Response.Body qualified as ResBody

data Request method path query headers body = Request
    { method  :: Maybe Method.ParseError
    , path    :: Maybe (Failure Path)
    , query   :: Maybe (Failure Query)
    , headers :: Maybe (Failure RequestHeaders)
    , body    :: Maybe ReqBody.ParseError
    }

data Response status headers body = Response
    { status  :: Maybe Status.ParseError
    , headers :: Maybe (Failure ResponseHeaders)
    , body    :: Maybe ResBody.ParseError
    }
