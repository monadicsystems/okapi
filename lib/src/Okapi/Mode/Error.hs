{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Okapi.Mode.Error where

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
    { method  :: Maybe (ErrorOf Method)
    , path    :: Maybe (ErrorOf Path)
    , query   :: Maybe (ErrorOf Query)
    , headers :: Maybe (ErrorOf Request.Headers)
    , body    :: Maybe (ErrorOf Request.Body)
    }

data Response status headers body = Response
    { status  :: Maybe (ErrorOf Status)
    , headers :: Maybe (ErrorOf Response.Headers)
    , body    :: Maybe (ErrorOf Response.Body)
    }
