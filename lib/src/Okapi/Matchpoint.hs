{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

module Okapi.Matchpoint where

import Control.Natural (type (~>))
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as Text
import qualified Network.HTTP.Types as HTTP
import qualified Network.Wai as WAI
import Okapi.Request (Request)
import Okapi.Response (Response)

pattern Matchpoint :: HTTP.StdMethod -> [Text.Text] -> HTTP.Query -> LBS.ByteString -> HTTP.RequestHeaders -> Request
pattern Matchpoint method path query body headers <- (method, path, query, body, headers)

type Server m = forall m. Monad m => Request -> m Response

instantiate :: Monad m => (m ~> IO) -> Server m -> WAI.Application
instantiate transformer server waiRequest respond = do
  let Right method = HTTP.parseMethod $ WAI.requestMethod waiRequest
      path = WAI.pathInfo waiRequest
      query = WAI.queryString waiRequest
      headers = WAI.requestHeaders waiRequest
  body <- WAI.strictRequestBody waiRequest
  let request = (method, path, query, body, headers)
  transformer $ server request
