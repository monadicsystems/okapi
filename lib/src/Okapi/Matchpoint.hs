{-# LANGUAGE PatternSynonyms #-}

module Okapi.Matchpoint where

import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as Text
import qualified Network.HTTP.Types as HTTP
import Okapi.Request (Request)

pattern Matchpoint :: HTTP.StdMethod -> [Text.Text] -> HTTP.Query -> LBS.ByteString -> HTTP.RequestHeaders -> Request
pattern Matchpoint method path query body headers <- (method, path, query, body, headers)
