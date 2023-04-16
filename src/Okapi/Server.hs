{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Okapi.Server where

import qualified Control.Monad.Par as Par
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.Text as Text
import qualified Network.HTTP.Types as HTTP
import qualified Network.Wai as WAI
import qualified Okapi.Controller as Controller

data Info = Info
  { author :: Text.Text,
    name :: Text.Text
  }

data Server = Server
  { info :: Maybe Info,
    controllers ::
      [ ( HTTP.Method,
          [Text.Text],
          HTTP.Query,
          HTTP.RequestHeaders,
          LBS.ByteString
        ) ->
        Controller.Controller
      ],
    defaultResponse :: WAI.Response
  }

plan1 :: Controller.Plan IO Int Int Int Int Int
plan1 = undefined

plan2 :: Controller.Plan IO Text.Text Int Float Char Int
plan2 = undefined

plan3 :: Controller.Plan IO Int Int Int Int Int
plan3 = undefined

myServer =
  Server
    { info = Nothing,
      controllers =
        [ Controller.make plan2,
          Controller.make plan1
        ],
      defaultResponse = WAI.responseLBS HTTP.status404 [] "Not Found"
    }

data Options = Options

genApplication ::
  Options ->
  Server ->
  WAI.Application
genApplication _ server request respond = case server.controllers of
  [] -> respond server.defaultResponse
  primedControllers -> do
    let method = WAI.requestMethod request
        path = WAI.pathInfo request
        query = WAI.queryString request
        headers = WAI.requestHeaders request
    body <- WAI.strictRequestBody request
    let input = (method, path, query, headers, body)
        -- NOTE: Can't run in parallel because Controller can't be instance of NFData
        -- outputs = Par.runPar $ do
        --   results <- mapM (Par.spawn . return . ($ input)) primedControllers
        --   mapM Par.get results
        outputs = map ($ input) primedControllers
    case getFirstRun outputs of
      Nothing -> respond server.defaultResponse
      Just action -> action >>= respond
  where
    getFirstRun :: [Controller.Controller] -> Maybe (IO WAI.Response)
    getFirstRun [] = Nothing
    getFirstRun (h : t) = case h of
      Controller.NoRun -> Nothing
      Controller.Run action -> Just action

genOpenAPISpec ::
  Server ->
  BS.ByteString
genOpenAPISpec = undefined

genJSClient ::
  Server ->
  BS.ByteString
genJSClient = undefined
