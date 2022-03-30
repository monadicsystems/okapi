{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Applicative ((<|>))
import Data.Aeson (ToJSON)
import Data.ByteString.Lazy (fromStrict)
import Data.Text
import Data.Text.Encoding (encodeUtf8)
import GHC.Generics (Generic)
import Okapi

main :: IO ()
main = print "todo"
