{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}

module Okapi.Record.Failure where

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
    { method  :: Maybe (Failure Method)
    , path    :: Maybe (Failure Path)
    , query   :: Maybe (Failure Query)
    , headers :: Maybe (Failure Request.Headers)
    , body    :: Maybe (Failure Request.Body)
    }

data Response status headers body = Response
    { status  :: Maybe (Failure Status)
    , headers :: Maybe (Failure Response.Headers)
    , body    :: Maybe (Failure Response.Body)
    }
