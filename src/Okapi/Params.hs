{-# LANGUAGE LinearTypes #-}

module Okapi.Params where

data Params p q h b r = Params
  { {-method :: HTTP.Method,-}
    path :: p,
    query :: q,
    headers :: h,
    body :: b,
    responder :: r
  }
