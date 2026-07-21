{-# LANGUAGE NoFieldSelectors #-}

module Okapi.Request.Data (Request (..)) where

data Request method path query headers body = Request
    { method  :: method
    , path    :: path
    , query   :: query
    , headers :: headers
    , body    :: body
    }
