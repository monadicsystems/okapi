{-# LANGUAGE NoFieldSelectors #-}

module Okapi.Response.Data (Response (..)) where

data Response status headers body = Response
    { status  :: status
    , headers :: headers
    , body    :: body
    }
