{-# LANGUAGE NoFieldSelectors #-}

module Okapi.Request.Result (Request (..)) where

import Okapi.HTTP.Tree (Failure, ForRequest)
import Okapi.HTTP.Headers (Headers)
import Okapi.HTTP.Request.Method qualified as Method
import Okapi.HTTP.Request.Path (Path)
import Okapi.HTTP.Request.Query (Query)

data Request method path query headers body = Request
    { method  :: Either Method.ParseError method
    , path    :: Either (Failure Path) path
    , query   :: Either (Failure Query) query
    , headers :: Either (Failure (Headers ForRequest)) headers
    , body    :: body
    }
