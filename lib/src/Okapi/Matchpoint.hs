{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

module Okapi.Matchpoint where

import Control.Natural (type (~>))
import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as Text
import Network.HTTP.Types qualified as HTTP
import Network.Wai qualified as WAI
import Okapi.Request (Request)
import Okapi.Response (Response)
import Okapi.Response qualified as Response

pattern Matchpoint :: HTTP.StdMethod -> [Text.Text] -> HTTP.Query -> LBS.ByteString -> HTTP.RequestHeaders -> Request
pattern Matchpoint method path query body headers <- (method, path, query, body, headers)

type Server m = Monad m => Request -> m Response

instantiate :: Monad m => (m ~> IO) -> Server m -> WAI.Application
instantiate transformer server waiRequest respond = do
  let Right method = HTTP.parseMethod $ WAI.requestMethod waiRequest
      path = WAI.pathInfo waiRequest
      query = WAI.queryString waiRequest
      headers = WAI.requestHeaders waiRequest
  body <- WAI.strictRequestBody waiRequest
  let request = (method, path, query, body, headers)
  response <- transformer $ server request
  respond $ Response.toWaiResponse response
