{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Okapi.Mode.Data where

data Request method path query headers body = Request
  { method :: method
  , path :: path
  , query :: query
  , headers :: headers
  , body :: body
  }

data Response status headers body = Response
  { status :: status
  , headers :: headers
  , body :: body
  }
