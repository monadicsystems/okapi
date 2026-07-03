{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}

-- | Records that bundle the 'Okapi.Tree.SymTree' codecs for each part of an
--   HTTP request or response into a single value.
module Okapi.Record.Tree where

import Okapi.Tree (SymTree)
import Okapi.HTTP.Request.Method (Method)
import Okapi.HTTP.Request.Path (Path)
import Okapi.HTTP.Request.Query (Query)
import Okapi.HTTP.Request.Headers qualified as Request (Headers)
import Okapi.HTTP.Request.Body qualified as Request (Body)
import Okapi.HTTP.Response.Status (Status)
import Okapi.HTTP.Response.Headers qualified as Response (Headers)
import Okapi.HTTP.Response.Body qualified as Response (Body)

-- | Codecs for every part of an HTTP request.
data Request method path query headers body = Request
    { method  :: Method method
    , path    :: SymTree Path path
    , query   :: SymTree Query query
    , headers :: SymTree Request.Headers headers
    , body    :: SymTree Request.Body body
    }

-- | Codecs for every part of an HTTP response.
data Response status headers body = Response
    { status  :: Status status
    , headers :: SymTree Response.Headers headers
    , body    :: SymTree Response.Body body
    }
