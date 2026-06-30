{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Okapi.Mode.Result where

import Okapi.Leaf (ErrorOf)
import Okapi.HTTP.Request.Method (Method)
import Okapi.HTTP.Request.Path (Path)
import Okapi.HTTP.Request.Query (Query)
import Okapi.HTTP.Request.Headers qualified as Request (Headers)
import Okapi.HTTP.Request.Body qualified as Request (Body)
import Okapi.HTTP.Response.Status (Status)
import Okapi.HTTP.Response.Headers qualified as Response (Headers)
import Okapi.HTTP.Response.Body qualified as Response (Body)

data Request method path query headers body = Request
    { method  :: Either (ErrorOf Method) method
    , path    :: Either (ErrorOf Path) path
    , query   :: Either (ErrorOf Query) query
    , headers :: Either (ErrorOf Request.Headers) headers
    , body    :: Either (ErrorOf Request.Body) body
    }

data Response status headers body = Response
    { status  :: Either (ErrorOf Status) status
    , headers :: Either (ErrorOf Response.Headers) headers
    , body    :: Either (ErrorOf Response.Body) body
    }
