{-# LANGUAGE NoFieldSelectors #-}

module Okapi.Data.Request (Request (..)) where

data Request method path query headers body = Request
    { method  :: method
    , path    :: path
    , query   :: query
    , headers :: headers
    , body    :: body
    }
