{-# LANGUAGE NoFieldSelectors #-}

module Okapi.HTTP.Response (
    response,
    response100,
    response101,
    response200,
    response201,
    response202,
    response203,
    response204,
    response205,
    response206,
    response300,
    response301,
    response302,
    response303,
    response304,
    response305,
    response307,
    response308,
    response400,
    response401,
    response402,
    response403,
    response404,
    response405,
    response406,
    response407,
    response408,
    response409,
    response410,
    response411,
    response412,
    response413,
    response414,
    response415,
    response416,
    response417,
    response418,
    response422,
    response428,
    response429,
    response431,
    response500,
    response501,
    response502,
    response503,
    response504,
    response505,
    response511,
    headers,
    body,
    extractWaiResBody,
    parser',
    resultToValue,
    resultToError,
    parser,
    printer,
) where

import Data.ByteString.Builder qualified as Builder
import Data.ByteString.Lazy qualified as LBS
import Network.HTTP.Types qualified as HTTP
import Network.Wai qualified as Wai
import Network.Wai.Internal qualified as WaiI
import Okapi.Record.Tree (Response (..))
import Okapi.Record.Tree qualified as Tree
import Okapi.Record.Failure qualified as Error
import Okapi.Record.Result qualified as Result
import Okapi.Record.Data qualified as Data
import Okapi.HTTP.Response.Body (Body)
import Okapi.HTTP.Response.Body qualified as Body
import Okapi.HTTP.Response.Headers (Headers)
import Okapi.HTTP.Response.Headers qualified as Headers
import Okapi.HTTP.Response.Status (KnownStatus)
import Okapi.HTTP.Response.Status qualified as Status
import Okapi.Tree (SymTree)

response :: Tree.Response HTTP.Status HTTP.ResponseHeaders (IO LBS.ByteString)
response = Response
    { status  = Status.raw
    , headers = Headers.raw
    , body    = Body.raw
    }

response100 :: Tree.Response (Status.KnownStatus 100) HTTP.ResponseHeaders (IO LBS.ByteString)
response100 = Response
    { status  = Status.status 100
    , headers = Headers.raw
    , body    = Body.raw
    }

response101 :: Tree.Response (Status.KnownStatus 101) HTTP.ResponseHeaders (IO LBS.ByteString)
response101 = Response
    { status  = Status.status 101
    , headers = Headers.raw
    , body    = Body.raw
    }

response200 :: Tree.Response (Status.KnownStatus 200) HTTP.ResponseHeaders (IO LBS.ByteString)
response200 = Response
    { status  = Status.status 200
    , headers = Headers.raw
    , body    = Body.raw
    }

response201 :: Tree.Response (Status.KnownStatus 201) HTTP.ResponseHeaders (IO LBS.ByteString)
response201 = Response
    { status  = Status.status 201
    , headers = Headers.raw
    , body    = Body.raw
    }

response202 :: Tree.Response (Status.KnownStatus 202) HTTP.ResponseHeaders (IO LBS.ByteString)
response202 = Response
    { status  = Status.status 202
    , headers = Headers.raw
    , body    = Body.raw
    }

response203 :: Tree.Response (Status.KnownStatus 203) HTTP.ResponseHeaders (IO LBS.ByteString)
response203 = Response
    { status  = Status.status 203
    , headers = Headers.raw
    , body    = Body.raw
    }

response204 :: Tree.Response (Status.KnownStatus 204) HTTP.ResponseHeaders (IO LBS.ByteString)
response204 = Response
    { status  = Status.status 204
    , headers = Headers.raw
    , body    = Body.raw
    }

response205 :: Tree.Response (Status.KnownStatus 205) HTTP.ResponseHeaders (IO LBS.ByteString)
response205 = Response
    { status  = Status.status 205
    , headers = Headers.raw
    , body    = Body.raw
    }

response206 :: Tree.Response (Status.KnownStatus 206) HTTP.ResponseHeaders (IO LBS.ByteString)
response206 = Response
    { status  = Status.status 206
    , headers = Headers.raw
    , body    = Body.raw
    }

response300 :: Tree.Response (Status.KnownStatus 300) HTTP.ResponseHeaders (IO LBS.ByteString)
response300 = Response
    { status  = Status.status 300
    , headers = Headers.raw
    , body    = Body.raw
    }

response301 :: Tree.Response (Status.KnownStatus 301) HTTP.ResponseHeaders (IO LBS.ByteString)
response301 = Response
    { status  = Status.status 301
    , headers = Headers.raw
    , body    = Body.raw
    }

response302 :: Tree.Response (Status.KnownStatus 302) HTTP.ResponseHeaders (IO LBS.ByteString)
response302 = Response
    { status  = Status.status 302
    , headers = Headers.raw
    , body    = Body.raw
    }

response303 :: Tree.Response (Status.KnownStatus 303) HTTP.ResponseHeaders (IO LBS.ByteString)
response303 = Response
    { status  = Status.status 303
    , headers = Headers.raw
    , body    = Body.raw
    }

response304 :: Tree.Response (Status.KnownStatus 304) HTTP.ResponseHeaders (IO LBS.ByteString)
response304 = Response
    { status  = Status.status 304
    , headers = Headers.raw
    , body    = Body.raw
    }

response305 :: Tree.Response (Status.KnownStatus 305) HTTP.ResponseHeaders (IO LBS.ByteString)
response305 = Response
    { status  = Status.status 305
    , headers = Headers.raw
    , body    = Body.raw
    }

response307 :: Tree.Response (Status.KnownStatus 307) HTTP.ResponseHeaders (IO LBS.ByteString)
response307 = Response
    { status  = Status.status 307
    , headers = Headers.raw
    , body    = Body.raw
    }

response308 :: Tree.Response (Status.KnownStatus 308) HTTP.ResponseHeaders (IO LBS.ByteString)
response308 = Response
    { status  = Status.status 308
    , headers = Headers.raw
    , body    = Body.raw
    }

response400 :: Tree.Response (Status.KnownStatus 400) HTTP.ResponseHeaders (IO LBS.ByteString)
response400 = Response
    { status  = Status.status 400
    , headers = Headers.raw
    , body    = Body.raw
    }

response401 :: Tree.Response (Status.KnownStatus 401) HTTP.ResponseHeaders (IO LBS.ByteString)
response401 = Response
    { status  = Status.status 401
    , headers = Headers.raw
    , body    = Body.raw
    }

response402 :: Tree.Response (Status.KnownStatus 402) HTTP.ResponseHeaders (IO LBS.ByteString)
response402 = Response
    { status  = Status.status 402
    , headers = Headers.raw
    , body    = Body.raw
    }

response403 :: Tree.Response (Status.KnownStatus 403) HTTP.ResponseHeaders (IO LBS.ByteString)
response403 = Response
    { status  = Status.status 403
    , headers = Headers.raw
    , body    = Body.raw
    }

response404 :: Tree.Response (Status.KnownStatus 404) HTTP.ResponseHeaders (IO LBS.ByteString)
response404 = Response
    { status  = Status.status 404
    , headers = Headers.raw
    , body    = Body.raw
    }

response405 :: Tree.Response (Status.KnownStatus 405) HTTP.ResponseHeaders (IO LBS.ByteString)
response405 = Response
    { status  = Status.status 405
    , headers = Headers.raw
    , body    = Body.raw
    }

response406 :: Tree.Response (Status.KnownStatus 406) HTTP.ResponseHeaders (IO LBS.ByteString)
response406 = Response
    { status  = Status.status 406
    , headers = Headers.raw
    , body    = Body.raw
    }

response407 :: Tree.Response (Status.KnownStatus 407) HTTP.ResponseHeaders (IO LBS.ByteString)
response407 = Response
    { status  = Status.status 407
    , headers = Headers.raw
    , body    = Body.raw
    }

response408 :: Tree.Response (Status.KnownStatus 408) HTTP.ResponseHeaders (IO LBS.ByteString)
response408 = Response
    { status  = Status.status 408
    , headers = Headers.raw
    , body    = Body.raw
    }

response409 :: Tree.Response (Status.KnownStatus 409) HTTP.ResponseHeaders (IO LBS.ByteString)
response409 = Response
    { status  = Status.status 409
    , headers = Headers.raw
    , body    = Body.raw
    }

response410 :: Tree.Response (Status.KnownStatus 410) HTTP.ResponseHeaders (IO LBS.ByteString)
response410 = Response
    { status  = Status.status 410
    , headers = Headers.raw
    , body    = Body.raw
    }

response411 :: Tree.Response (Status.KnownStatus 411) HTTP.ResponseHeaders (IO LBS.ByteString)
response411 = Response
    { status  = Status.status 411
    , headers = Headers.raw
    , body    = Body.raw
    }

response412 :: Tree.Response (Status.KnownStatus 412) HTTP.ResponseHeaders (IO LBS.ByteString)
response412 = Response
    { status  = Status.status 412
    , headers = Headers.raw
    , body    = Body.raw
    }

response413 :: Tree.Response (Status.KnownStatus 413) HTTP.ResponseHeaders (IO LBS.ByteString)
response413 = Response
    { status  = Status.status 413
    , headers = Headers.raw
    , body    = Body.raw
    }

response414 :: Tree.Response (Status.KnownStatus 414) HTTP.ResponseHeaders (IO LBS.ByteString)
response414 = Response
    { status  = Status.status 414
    , headers = Headers.raw
    , body    = Body.raw
    }

response415 :: Tree.Response (Status.KnownStatus 415) HTTP.ResponseHeaders (IO LBS.ByteString)
response415 = Response
    { status  = Status.status 415
    , headers = Headers.raw
    , body    = Body.raw
    }

response416 :: Tree.Response (Status.KnownStatus 416) HTTP.ResponseHeaders (IO LBS.ByteString)
response416 = Response
    { status  = Status.status 416
    , headers = Headers.raw
    , body    = Body.raw
    }

response417 :: Tree.Response (Status.KnownStatus 417) HTTP.ResponseHeaders (IO LBS.ByteString)
response417 = Response
    { status  = Status.status 417
    , headers = Headers.raw
    , body    = Body.raw
    }

response418 :: Tree.Response (Status.KnownStatus 418) HTTP.ResponseHeaders (IO LBS.ByteString)
response418 = Response
    { status  = Status.status 418
    , headers = Headers.raw
    , body    = Body.raw
    }

response422 :: Tree.Response (Status.KnownStatus 422) HTTP.ResponseHeaders (IO LBS.ByteString)
response422 = Response
    { status  = Status.status 422
    , headers = Headers.raw
    , body    = Body.raw
    }

response428 :: Tree.Response (Status.KnownStatus 428) HTTP.ResponseHeaders (IO LBS.ByteString)
response428 = Response
    { status  = Status.status 428
    , headers = Headers.raw
    , body    = Body.raw
    }

response429 :: Tree.Response (Status.KnownStatus 429) HTTP.ResponseHeaders (IO LBS.ByteString)
response429 = Response
    { status  = Status.status 429
    , headers = Headers.raw
    , body    = Body.raw
    }

response431 :: Tree.Response (Status.KnownStatus 431) HTTP.ResponseHeaders (IO LBS.ByteString)
response431 = Response
    { status  = Status.status 431
    , headers = Headers.raw
    , body    = Body.raw
    }

response500 :: Tree.Response (Status.KnownStatus 500) HTTP.ResponseHeaders (IO LBS.ByteString)
response500 = Response
    { status  = Status.status 500
    , headers = Headers.raw
    , body    = Body.raw
    }

response501 :: Tree.Response (Status.KnownStatus 501) HTTP.ResponseHeaders (IO LBS.ByteString)
response501 = Response
    { status  = Status.status 501
    , headers = Headers.raw
    , body    = Body.raw
    }

response502 :: Tree.Response (Status.KnownStatus 502) HTTP.ResponseHeaders (IO LBS.ByteString)
response502 = Response
    { status  = Status.status 502
    , headers = Headers.raw
    , body    = Body.raw
    }

response503 :: Tree.Response (Status.KnownStatus 503) HTTP.ResponseHeaders (IO LBS.ByteString)
response503 = Response
    { status  = Status.status 503
    , headers = Headers.raw
    , body    = Body.raw
    }

response504 :: Tree.Response (Status.KnownStatus 504) HTTP.ResponseHeaders (IO LBS.ByteString)
response504 = Response
    { status  = Status.status 504
    , headers = Headers.raw
    , body    = Body.raw
    }

response505 :: Tree.Response (Status.KnownStatus 505) HTTP.ResponseHeaders (IO LBS.ByteString)
response505 = Response
    { status  = Status.status 505
    , headers = Headers.raw
    , body    = Body.raw
    }

response511 :: Tree.Response (Status.KnownStatus 511) HTTP.ResponseHeaders (IO LBS.ByteString)
response511 = Response
    { status  = Status.status 511
    , headers = Headers.raw
    , body    = Body.raw
    }
headers ::
    SymTree Headers headers ->
    Tree.Response status HTTP.ResponseHeaders body ->
    Tree.Response status headers body
headers c r = r { headers = c }

body ::
    Body body ->
    Tree.Response status headers (IO LBS.ByteString) ->
    Tree.Response status headers body
body c r = r { body = c }

extractWaiResBody :: Wai.Response -> IO LBS.ByteString
extractWaiResBody (WaiI.ResponseBuilder _ _ b) = pure (Builder.toLazyByteString b)
extractWaiResBody _                            = pure LBS.empty

parser' ::
    Tree.Response status headers body ->
    Wai.Response ->
    IO (Result.Response status headers body)
parser' codec waiRes = do
    let httpStatus = Wai.responseStatus waiRes
        waiHeaders = Wai.responseHeaders waiRes
        sr         = Status.parse codec.status httpStatus
        (hr, _)    = Headers.parser codec.headers waiHeaders
        br         = Body.parser codec.body (extractWaiResBody waiRes)
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

parser ::
    Tree.Response status headers body ->
    Wai.Response ->
    IO (Either (Error.Response status headers body) (Data.Response status headers body))
parser res waiRes = do
    rr <- parser' res waiRes
    pure $ maybe (Left (resultToError rr)) Right (resultToValue rr)

printer ::
    Tree.Response status headers body ->
    Data.Response status headers body ->
    IO Wai.Response
printer codec value = do
    bodyBytes <- Body.printer codec.body value.body
    let responseHeaders = Headers.printer codec.headers value.headers
    pure $ Wai.responseLBS (Status.print codec.status value.status) responseHeaders bodyBytes
