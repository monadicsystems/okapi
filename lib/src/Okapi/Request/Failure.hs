{-# LANGUAGE NoFieldSelectors #-}

module Okapi.Request.Failure (Request (..)) where

import Okapi.HTTP.Tree (Failure, ForRequest)
import Okapi.HTTP.Headers (Headers)
import Okapi.HTTP.Request.Method qualified as Method
import Okapi.HTTP.Request.Path (Path)
import Okapi.HTTP.Request.Query (Query)

data Request method path query headers body = Request
    { method  :: Maybe Method.ParseError
    , path    :: Maybe (Failure Path)
    , query   :: Maybe (Failure Query)
    , headers :: Maybe (Failure (Headers ForRequest))
    }
