{-# LANGUAGE DataKinds #-}

module Okapi.Kind where

data METHOD = GET | POST | PUT | DELETE

data STATUS = S200 | S404 | S500
