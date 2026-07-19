{-# LANGUAGE NoFieldSelectors #-}

module Okapi.Data.Response (Response (..)) where

data Response status headers body = Response
    { status  :: status
    , headers :: headers
    , body    :: body
    }
