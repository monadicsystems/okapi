{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module Okapi.DSL where

class DSL expr input error output where
  eval :: expr -> input -> (Either error output, input)
