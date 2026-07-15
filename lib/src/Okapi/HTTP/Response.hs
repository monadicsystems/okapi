{-# LANGUAGE NoFieldSelectors #-}

module Okapi.HTTP.Response (
    Response (..),
    res,
    res100,
    res101,
    res200,
    res201,
    res202,
    res203,
    res204,
    res205,
    res206,
    res300,
    res301,
    res302,
    res303,
    res304,
    res305,
    res307,
    res308,
    res400,
    res401,
    res402,
    res403,
    res404,
    res405,
    res406,
    res407,
    res408,
    res409,
    res410,
    res411,
    res412,
    res413,
    res414,
    res415,
    res416,
    res417,
    res418,
    res422,
    res428,
    res429,
    res431,
    res500,
    res501,
    res502,
    res503,
    res504,
    res505,
    res511,
    headers,
    body,
    extractWaiResBody,
    parser',
    resultToValue,
    resultToError,
    parser,
    printer,

    -- * Header combinators (re-exported from "Okapi.HTTP.Headers")
    field,
    field',
    field_,
    contentType,
    fieldStruct,
    fieldBareItem,
    fieldItem,
    fieldList,
    fieldDict,
    setCookie,
    MediaType (..),

    -- * Body combinators (re-exported from "Okapi.HTTP.Body")
    json,
    jsonValue,
    noContent,
    None (..),
) where

import Data.ByteString.Builder qualified as Builder
import Data.ByteString.Lazy qualified as LBS
import Network.HTTP.Types qualified as Types
import Network.Wai qualified as Wai
import Network.Wai.Internal qualified as WaiI
import Okapi.Record.Failure qualified as Error
import Okapi.Record.Result qualified as Result
import Okapi.Record.Data qualified as Data
import Okapi.HTTP.Headers (MediaType (..))
import Okapi.HTTP.Headers qualified as Headers
import Okapi.HTTP.Headers
    ( field, field', field_, contentType
    , fieldStruct, fieldBareItem, fieldItem, fieldList, fieldDict
    )
import Okapi.HTTP.Body qualified as Body
import Okapi.HTTP.Body (json, jsonValue, noContent, None (..))
import Okapi.HTTP.Response.Body (ResponseBody)
import Okapi.HTTP.Response.Headers (ResponseHeaders, setCookie)
import Okapi.HTTP.Response.Status (KnownStatus)
import Okapi.HTTP.Response.Status qualified as Status
import Okapi.Tree (SymTree)

-- | Codecs for every part of an HTTP response.
data Response status headers body = Response
    { status  :: Status.Status status
    , headers :: SymTree ResponseHeaders headers
    , body    :: ResponseBody body
    }

res :: Response Types.Status Types.ResponseHeaders (IO LBS.ByteString)
res = Response
    { status  = Status.raw
    , headers = Headers.raw
    , body    = Body.raw
    }

res100 :: Response (Status.KnownStatus 100) Types.ResponseHeaders (IO LBS.ByteString)
res100 = Response
    { status  = Status.status 100
    , headers = Headers.raw
    , body    = Body.raw
    }

res101 :: Response (Status.KnownStatus 101) Types.ResponseHeaders (IO LBS.ByteString)
res101 = Response
    { status  = Status.status 101
    , headers = Headers.raw
    , body    = Body.raw
    }

res200 :: Response (Status.KnownStatus 200) Types.ResponseHeaders (IO LBS.ByteString)
res200 = Response
    { status  = Status.status 200
    , headers = Headers.raw
    , body    = Body.raw
    }

res201 :: Response (Status.KnownStatus 201) Types.ResponseHeaders (IO LBS.ByteString)
res201 = Response
    { status  = Status.status 201
    , headers = Headers.raw
    , body    = Body.raw
    }

res202 :: Response (Status.KnownStatus 202) Types.ResponseHeaders (IO LBS.ByteString)
res202 = Response
    { status  = Status.status 202
    , headers = Headers.raw
    , body    = Body.raw
    }

res203 :: Response (Status.KnownStatus 203) Types.ResponseHeaders (IO LBS.ByteString)
res203 = Response
    { status  = Status.status 203
    , headers = Headers.raw
    , body    = Body.raw
    }

res204 :: Response (Status.KnownStatus 204) Types.ResponseHeaders (IO LBS.ByteString)
res204 = Response
    { status  = Status.status 204
    , headers = Headers.raw
    , body    = Body.raw
    }

res205 :: Response (Status.KnownStatus 205) Types.ResponseHeaders (IO LBS.ByteString)
res205 = Response
    { status  = Status.status 205
    , headers = Headers.raw
    , body    = Body.raw
    }

res206 :: Response (Status.KnownStatus 206) Types.ResponseHeaders (IO LBS.ByteString)
res206 = Response
    { status  = Status.status 206
    , headers = Headers.raw
    , body    = Body.raw
    }

res300 :: Response (Status.KnownStatus 300) Types.ResponseHeaders (IO LBS.ByteString)
res300 = Response
    { status  = Status.status 300
    , headers = Headers.raw
    , body    = Body.raw
    }

res301 :: Response (Status.KnownStatus 301) Types.ResponseHeaders (IO LBS.ByteString)
res301 = Response
    { status  = Status.status 301
    , headers = Headers.raw
    , body    = Body.raw
    }

res302 :: Response (Status.KnownStatus 302) Types.ResponseHeaders (IO LBS.ByteString)
res302 = Response
    { status  = Status.status 302
    , headers = Headers.raw
    , body    = Body.raw
    }

res303 :: Response (Status.KnownStatus 303) Types.ResponseHeaders (IO LBS.ByteString)
res303 = Response
    { status  = Status.status 303
    , headers = Headers.raw
    , body    = Body.raw
    }

res304 :: Response (Status.KnownStatus 304) Types.ResponseHeaders (IO LBS.ByteString)
res304 = Response
    { status  = Status.status 304
    , headers = Headers.raw
    , body    = Body.raw
    }

res305 :: Response (Status.KnownStatus 305) Types.ResponseHeaders (IO LBS.ByteString)
res305 = Response
    { status  = Status.status 305
    , headers = Headers.raw
    , body    = Body.raw
    }

res307 :: Response (Status.KnownStatus 307) Types.ResponseHeaders (IO LBS.ByteString)
res307 = Response
    { status  = Status.status 307
    , headers = Headers.raw
    , body    = Body.raw
    }

res308 :: Response (Status.KnownStatus 308) Types.ResponseHeaders (IO LBS.ByteString)
res308 = Response
    { status  = Status.status 308
    , headers = Headers.raw
    , body    = Body.raw
    }

res400 :: Response (Status.KnownStatus 400) Types.ResponseHeaders (IO LBS.ByteString)
res400 = Response
    { status  = Status.status 400
    , headers = Headers.raw
    , body    = Body.raw
    }

res401 :: Response (Status.KnownStatus 401) Types.ResponseHeaders (IO LBS.ByteString)
res401 = Response
    { status  = Status.status 401
    , headers = Headers.raw
    , body    = Body.raw
    }

res402 :: Response (Status.KnownStatus 402) Types.ResponseHeaders (IO LBS.ByteString)
res402 = Response
    { status  = Status.status 402
    , headers = Headers.raw
    , body    = Body.raw
    }

res403 :: Response (Status.KnownStatus 403) Types.ResponseHeaders (IO LBS.ByteString)
res403 = Response
    { status  = Status.status 403
    , headers = Headers.raw
    , body    = Body.raw
    }

res404 :: Response (Status.KnownStatus 404) Types.ResponseHeaders (IO LBS.ByteString)
res404 = Response
    { status  = Status.status 404
    , headers = Headers.raw
    , body    = Body.raw
    }

res405 :: Response (Status.KnownStatus 405) Types.ResponseHeaders (IO LBS.ByteString)
res405 = Response
    { status  = Status.status 405
    , headers = Headers.raw
    , body    = Body.raw
    }

res406 :: Response (Status.KnownStatus 406) Types.ResponseHeaders (IO LBS.ByteString)
res406 = Response
    { status  = Status.status 406
    , headers = Headers.raw
    , body    = Body.raw
    }

res407 :: Response (Status.KnownStatus 407) Types.ResponseHeaders (IO LBS.ByteString)
res407 = Response
    { status  = Status.status 407
    , headers = Headers.raw
    , body    = Body.raw
    }

res408 :: Response (Status.KnownStatus 408) Types.ResponseHeaders (IO LBS.ByteString)
res408 = Response
    { status  = Status.status 408
    , headers = Headers.raw
    , body    = Body.raw
    }

res409 :: Response (Status.KnownStatus 409) Types.ResponseHeaders (IO LBS.ByteString)
res409 = Response
    { status  = Status.status 409
    , headers = Headers.raw
    , body    = Body.raw
    }

res410 :: Response (Status.KnownStatus 410) Types.ResponseHeaders (IO LBS.ByteString)
res410 = Response
    { status  = Status.status 410
    , headers = Headers.raw
    , body    = Body.raw
    }

res411 :: Response (Status.KnownStatus 411) Types.ResponseHeaders (IO LBS.ByteString)
res411 = Response
    { status  = Status.status 411
    , headers = Headers.raw
    , body    = Body.raw
    }

res412 :: Response (Status.KnownStatus 412) Types.ResponseHeaders (IO LBS.ByteString)
res412 = Response
    { status  = Status.status 412
    , headers = Headers.raw
    , body    = Body.raw
    }

res413 :: Response (Status.KnownStatus 413) Types.ResponseHeaders (IO LBS.ByteString)
res413 = Response
    { status  = Status.status 413
    , headers = Headers.raw
    , body    = Body.raw
    }

res414 :: Response (Status.KnownStatus 414) Types.ResponseHeaders (IO LBS.ByteString)
res414 = Response
    { status  = Status.status 414
    , headers = Headers.raw
    , body    = Body.raw
    }

res415 :: Response (Status.KnownStatus 415) Types.ResponseHeaders (IO LBS.ByteString)
res415 = Response
    { status  = Status.status 415
    , headers = Headers.raw
    , body    = Body.raw
    }

res416 :: Response (Status.KnownStatus 416) Types.ResponseHeaders (IO LBS.ByteString)
res416 = Response
    { status  = Status.status 416
    , headers = Headers.raw
    , body    = Body.raw
    }

res417 :: Response (Status.KnownStatus 417) Types.ResponseHeaders (IO LBS.ByteString)
res417 = Response
    { status  = Status.status 417
    , headers = Headers.raw
    , body    = Body.raw
    }

res418 :: Response (Status.KnownStatus 418) Types.ResponseHeaders (IO LBS.ByteString)
res418 = Response
    { status  = Status.status 418
    , headers = Headers.raw
    , body    = Body.raw
    }

res422 :: Response (Status.KnownStatus 422) Types.ResponseHeaders (IO LBS.ByteString)
res422 = Response
    { status  = Status.status 422
    , headers = Headers.raw
    , body    = Body.raw
    }

res428 :: Response (Status.KnownStatus 428) Types.ResponseHeaders (IO LBS.ByteString)
res428 = Response
    { status  = Status.status 428
    , headers = Headers.raw
    , body    = Body.raw
    }

res429 :: Response (Status.KnownStatus 429) Types.ResponseHeaders (IO LBS.ByteString)
res429 = Response
    { status  = Status.status 429
    , headers = Headers.raw
    , body    = Body.raw
    }

res431 :: Response (Status.KnownStatus 431) Types.ResponseHeaders (IO LBS.ByteString)
res431 = Response
    { status  = Status.status 431
    , headers = Headers.raw
    , body    = Body.raw
    }

res500 :: Response (Status.KnownStatus 500) Types.ResponseHeaders (IO LBS.ByteString)
res500 = Response
    { status  = Status.status 500
    , headers = Headers.raw
    , body    = Body.raw
    }

res501 :: Response (Status.KnownStatus 501) Types.ResponseHeaders (IO LBS.ByteString)
res501 = Response
    { status  = Status.status 501
    , headers = Headers.raw
    , body    = Body.raw
    }

res502 :: Response (Status.KnownStatus 502) Types.ResponseHeaders (IO LBS.ByteString)
res502 = Response
    { status  = Status.status 502
    , headers = Headers.raw
    , body    = Body.raw
    }

res503 :: Response (Status.KnownStatus 503) Types.ResponseHeaders (IO LBS.ByteString)
res503 = Response
    { status  = Status.status 503
    , headers = Headers.raw
    , body    = Body.raw
    }

res504 :: Response (Status.KnownStatus 504) Types.ResponseHeaders (IO LBS.ByteString)
res504 = Response
    { status  = Status.status 504
    , headers = Headers.raw
    , body    = Body.raw
    }

res505 :: Response (Status.KnownStatus 505) Types.ResponseHeaders (IO LBS.ByteString)
res505 = Response
    { status  = Status.status 505
    , headers = Headers.raw
    , body    = Body.raw
    }

res511 :: Response (Status.KnownStatus 511) Types.ResponseHeaders (IO LBS.ByteString)
res511 = Response
    { status  = Status.status 511
    , headers = Headers.raw
    , body    = Body.raw
    }
headers ::
    SymTree ResponseHeaders headers ->
    Response status Types.ResponseHeaders body ->
    Response status headers body
headers c r = r { headers = c }

body ::
    ResponseBody body ->
    Response status headers (IO LBS.ByteString) ->
    Response status headers body
body c r = r { body = c }

extractWaiResBody :: Wai.Response -> IO LBS.ByteString
extractWaiResBody (WaiI.ResponseBuilder _ _ b) = pure (Builder.toLazyByteString b)
extractWaiResBody _                            = pure LBS.empty

parser' ::
    Response status headers body ->
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
    Response status headers body ->
    Wai.Response ->
    IO (Either (Error.Response status headers body) (Data.Response status headers body))
parser res waiRes = do
    rr <- parser' res waiRes
    pure $ maybe (Left (resultToError rr)) Right (resultToValue rr)

printer ::
    Response status headers body ->
    Data.Response status headers body ->
    IO Wai.Response
printer codec value = do
    bodyBytes <- Body.printer codec.body value.body
    let responseHeaders = Headers.printer codec.headers value.headers
    pure $ Wai.responseLBS (Status.print codec.status value.status) responseHeaders bodyBytes
