{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}

module Okapi.Record.Result where

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
    { method  :: Either Method.ParseError method
    , path    :: Either (Failure Path) path
    , query   :: Either (Failure Query) query
    , headers :: Either (Failure RequestHeaders) headers
    , body    :: Either ReqBody.ParseError body
    }

data Response status headers body = Response
    { status  :: Either Status.ParseError status
    , headers :: Either (Failure ResponseHeaders) headers
    , body    :: Either ResBody.ParseError body
    }
