{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

module Okapi.Patterns where

import qualified Data.ByteString as BS
import qualified Data.List as List
import Data.Text
import Okapi.Types
import Web.HttpApiData

pattern GET :: Method
pattern GET = "GET"

pattern POST :: Method
pattern POST = "POST"

pattern DELETE :: Method
pattern DELETE = "DELETE"

pattern OTHER :: Method -> Method
pattern OTHER method <-
  method
  where
    OTHER method = method

pattern PathParam :: (ToHttpApiData a, FromHttpApiData a) => a -> Text
pattern PathParam param <-
  (parseUrlPiece -> Right param)
  where
    PathParam param = toUrlPiece param

pattern HasQueryParam :: FromHttpApiData a => a -> Maybe QueryValue
pattern HasQueryParam value <- Just (QueryParam (parseQueryParam -> Right value))

pattern HasQueryFlag :: Maybe QueryValue
pattern HasQueryFlag <- Just QueryFlag

viewQuery :: Text -> Query -> (Maybe QueryValue, Query)
viewQuery name query = case List.lookup name query of
  Nothing -> (Nothing, query)
  Just value -> (Just value, List.delete (name, value) query)

viewHeaders :: HeaderName -> Headers -> (Maybe BS.ByteString, Headers)
viewHeaders name headers = case List.lookup name headers of
  Nothing -> (Nothing, headers)
  Just value -> (Just value, List.delete (name, value) headers)
