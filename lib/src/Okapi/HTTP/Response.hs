{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Okapi.HTTP.Response (
    response,
    status200,
    status201,
    status204,
    status404,
    status500,
    headers,
    body,
    extractWaiResBody,
    parseResponseResult,
    resultToValue,
    resultToError,
    parseResponse,
    printResponse,
) where

import Data.ByteString.Builder qualified as Builder
import Data.ByteString.Lazy qualified as LBS
import Network.HTTP.Types qualified as HTTP
import Network.Wai qualified as Wai
import Network.Wai.Internal qualified as WaiI
import Okapi.Mode.Tree (Response (..))
import Okapi.Mode.Tree qualified as Tree
import Okapi.Mode.Error qualified as Error
import Okapi.Mode.Result qualified as Result
import Okapi.Mode.Data qualified as Data
import Okapi.HTTP.Response.Body (Body)
import Okapi.HTTP.Response.Body qualified as Body
import Okapi.HTTP.Response.Headers (Headers)
import Okapi.HTTP.Response.Headers qualified as Headers
import Okapi.HTTP.Response.Status (S200, S201, S204, S404, S500)
import Okapi.HTTP.Response.Status qualified as Status
import Okapi.Tree (SymTree)

response :: Tree.Response HTTP.Status HTTP.ResponseHeaders (IO LBS.ByteString)
response = Response
    { status  = Status.raw
    , headers = Headers.raw
    , body    = Body.raw
    }

status200 :: Tree.Response S200 HTTP.ResponseHeaders (IO LBS.ByteString)
status200 = Response
    { status  = Status.status Status.S200
    , headers = Headers.raw
    , body    = Body.raw
    }

status201 :: Tree.Response S201 HTTP.ResponseHeaders (IO LBS.ByteString)
status201 = Response
    { status  = Status.status Status.S201
    , headers = Headers.raw
    , body    = Body.raw
    }

status204 :: Tree.Response S204 HTTP.ResponseHeaders (IO LBS.ByteString)
status204 = Response
    { status  = Status.status Status.S204
    , headers = Headers.raw
    , body    = Body.raw
    }

status404 :: Tree.Response S404 HTTP.ResponseHeaders (IO LBS.ByteString)
status404 = Response
    { status  = Status.status Status.S404
    , headers = Headers.raw
    , body    = Body.raw
    }

status500 :: Tree.Response S500 HTTP.ResponseHeaders (IO LBS.ByteString)
status500 = Response
    { status  = Status.status Status.S500
    , headers = Headers.raw
    , body    = Body.raw
    }

headers ::
    SymTree Headers headers ->
    Tree.Response status HTTP.ResponseHeaders body ->
    Tree.Response status headers body
headers c r = r { headers = c }

body ::
    SymTree Body body ->
    Tree.Response status headers (IO LBS.ByteString) ->
    Tree.Response status headers body
body c r = r { body = c }

extractWaiResBody :: Wai.Response -> IO LBS.ByteString
extractWaiResBody (WaiI.ResponseBuilder _ _ b) = pure (Builder.toLazyByteString b)
extractWaiResBody _                            = pure LBS.empty

parseResponseResult ::
    Tree.Response status headers body ->
    Wai.Response ->
    IO (Result.Response status headers body)
parseResponseResult codec waiRes = do
    let httpStatus = Wai.responseStatus waiRes
        waiHeaders = Wai.responseHeaders waiRes
        sr         = Status.parse codec.status httpStatus
        (hr, _)    = Headers.parse codec.headers waiHeaders
        (br, _)    = Body.parse codec.body (extractWaiResBody waiRes)
    pure $ Result.Response { status = sr, headers = hr, body = br }

resultToValue ::
    Result.Response status headers body ->
    Maybe (Data.Response status headers body)
resultToValue result = case (result.status, result.headers, result.body) of
    (Right status, Right headers, Right body) ->
        Just $ Data.Response { status = status, headers = headers, body = body }
    _ -> Nothing

resultToError ::
    Result.Response status headers body ->
    Error.Response status headers body
resultToError result = Error.Response
    { status  = either Just (const Nothing) result.status
    , headers = either Just (const Nothing) result.headers
    , body    = either Just (const Nothing) result.body
    }

parseResponse ::
    Tree.Response status headers body ->
    Wai.Response ->
    IO (Either (Error.Response status headers body) (Data.Response status headers body))
parseResponse res waiRes = do
    rr <- parseResponseResult res waiRes
    pure $ maybe (Left (resultToError rr)) Right (resultToValue rr)

printResponse ::
    Tree.Response status headers body ->
    Data.Response status headers body ->
    IO Wai.Response
printResponse codec value = do
    bodyBytes <- Body.print codec.body value.body
    let responseHeaders = Headers.print codec.headers value.headers
    pure $ Wai.responseLBS (Status.print codec.status value.status) responseHeaders bodyBytes
