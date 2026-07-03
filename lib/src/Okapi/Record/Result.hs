{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}

module Okapi.Record.Result where

import Okapi.Tree (Failure)
import Okapi.HTTP.Request.Method (Method)
import Okapi.HTTP.Request.Path (Path)
import Okapi.HTTP.Request.Query (Query)
import Okapi.HTTP.Request.Headers qualified as Request (Headers)
import Okapi.HTTP.Request.Body qualified as Request (Body)
import Okapi.HTTP.Response.Status (Status)
import Okapi.HTTP.Response.Headers qualified as Response (Headers)
import Okapi.HTTP.Response.Body qualified as Response (Body)

data Request method path query headers body = Request
    { method  :: Either (Failure Method) method
    , path    :: Either (Failure Path) path
    , query   :: Either (Failure Query) query
    , headers :: Either (Failure Request.Headers) headers
    , body    :: Either (Failure Request.Body) body
    }

data Response status headers body = Response
    { status  :: Either (Failure Status) status
    , headers :: Either (Failure Response.Headers) headers
    , body    :: Either (Failure Response.Body) body
    }
