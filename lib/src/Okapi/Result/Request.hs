{-# LANGUAGE NoFieldSelectors #-}

module Okapi.Result.Request (Request (..)) where

import Okapi.HTTP.Tree (Failure)
import Okapi.HTTP.Request.Method qualified as Method
import Okapi.HTTP.Request.Path (Path)
import Okapi.HTTP.Request.Query (Query)
import Okapi.HTTP.Request.Headers (RequestHeaders)
import Okapi.HTTP.Request.Body qualified as ReqBody

data Request method path query headers body = Request
    { method  :: Either Method.ParseError method
    , path    :: Either (Failure Path) path
    , query   :: Either (Failure Query) query
    , headers :: Either (Failure RequestHeaders) headers
    , body    :: Either ReqBody.ParseError body
    }
