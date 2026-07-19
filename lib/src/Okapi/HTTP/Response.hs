{-# LANGUAGE NoFieldSelectors #-}

module Okapi.HTTP.Response (
    Response (..),
    any,
    continue,
    switchingProtocols,
    ok,
    created,
    accepted,
    nonAuthoritative,
    noContent,
    resetContent,
    partialContent,
    multipleChoices,
    movedPermanently,
    found,
    seeOther,
    notModified,
    useProxy,
    temporaryRedirect,
    permanentRedirect,
    badRequest,
    unauthorized,
    paymentRequired,
    forbidden,
    notFound,
    methodNotAllowed,
    notAcceptable,
    proxyAuthenticationRequired,
    requestTimeout,
    conflict,
    gone,
    lengthRequired,
    preconditionFailed,
    requestEntityTooLarge,
    requestURITooLong,
    unsupportedMediaType,
    requestedRangeNotSatisfiable,
    expectationFailed,
    imATeapot,
    unprocessableEntity,
    preconditionRequired,
    tooManyRequests,
    requestHeaderFieldsTooLarge,
    internalServerError,
    notImplemented,
    badGateway,
    serviceUnavailable,
    gatewayTimeout,
    httpVersionNotSupported,
    networkAuthenticationRequired,
    headers,
    body,
    extractWaiResBody,
    parser',
    resultToValue,
    resultToError,
    parser,
    printer,

    -- * Side-pinned header combinator (re-exported from "Okapi.HTTP.Headers")
    setCookie,

    -- * Status singletons (re-exported from "Okapi.HTTP.Response.Status") -- the full set
    KnownStatus (..),
    S100, S101, S200, S201, S202, S203, S204, S205, S206,
    S300, S301, S302, S303, S304, S305, S307, S308,
    S400, S401, S402, S403, S404, S405, S406, S407, S408, S409,
    S410, S411, S412, S413, S414, S415, S416, S417, S418,
    S422, S428, S429, S431,
    S500, S501, S502, S503, S504, S505, S511,
    SomeKnownStatus (..),
    allKnownStatuses,

    -- * Set-Cookie attributes (re-exported from "Okapi.HTTP.Response.Headers.Attributes")
    attr,
    attr',
    secure,
    httpOnly,
) where

import Prelude hiding (any)
import Data.ByteString.Builder qualified as Builder
import Data.ByteString.Lazy qualified as LBS
import Network.HTTP.Types qualified as Types
import Network.Wai qualified as Wai
import Network.Wai.Internal qualified as WaiI
import Okapi.HTTP.Headers qualified as Headers
import Okapi.HTTP.Body qualified as Body
import Okapi.HTTP.Response.Body (ResponseBody)
import Okapi.HTTP.Response.Headers (ResponseHeaders, setCookie)
import Okapi.HTTP.Response.Status
    ( KnownStatus (..)
    , S100, S101, S200, S201, S202, S203, S204, S205, S206
    , S300, S301, S302, S303, S304, S305, S307, S308
    , S400, S401, S402, S403, S404, S405, S406, S407, S408, S409
    , S410, S411, S412, S413, S414, S415, S416, S417, S418
    , S422, S428, S429, S431
    , S500, S501, S502, S503, S504, S505, S511
    , SomeKnownStatus (..)
    , allKnownStatuses
    )
import Okapi.HTTP.Response.Status qualified as Status
import Okapi.HTTP.Response.Headers.Attributes (attr, attr', secure, httpOnly)
import Okapi.HTTP.Tree (SymTree)
import Okapi.Data.Response qualified as Data
import Okapi.Result.Response qualified as Result
import Okapi.Failure.Response qualified as Error

-- | Codecs for every part of an HTTP response.
data Response status headers body = Response
    { status  :: Status.Status status
    , headers :: SymTree ResponseHeaders headers
    , body    :: ResponseBody body
    }

any :: Response Types.Status Types.ResponseHeaders (IO LBS.ByteString)
any = Response
    { status  = Status.raw
    , headers = Headers.raw
    , body    = Body.raw
    }

continue :: Response S100 Types.ResponseHeaders (IO LBS.ByteString)
continue = Response
    { status  = Status.status 100
    , headers = Headers.raw
    , body    = Body.raw
    }

switchingProtocols :: Response S101 Types.ResponseHeaders (IO LBS.ByteString)
switchingProtocols = Response
    { status  = Status.status 101
    , headers = Headers.raw
    , body    = Body.raw
    }

ok :: Response S200 Types.ResponseHeaders (IO LBS.ByteString)
ok = Response
    { status  = Status.status 200
    , headers = Headers.raw
    , body    = Body.raw
    }

created :: Response S201 Types.ResponseHeaders (IO LBS.ByteString)
created = Response
    { status  = Status.status 201
    , headers = Headers.raw
    , body    = Body.raw
    }

accepted :: Response S202 Types.ResponseHeaders (IO LBS.ByteString)
accepted = Response
    { status  = Status.status 202
    , headers = Headers.raw
    , body    = Body.raw
    }

nonAuthoritative :: Response S203 Types.ResponseHeaders (IO LBS.ByteString)
nonAuthoritative = Response
    { status  = Status.status 203
    , headers = Headers.raw
    , body    = Body.raw
    }

noContent :: Response S204 Types.ResponseHeaders (IO Body.None)
noContent = Response
    { status  = Status.status 204
    , headers = Headers.raw
    , body    = Body.none
    }

resetContent :: Response S205 Types.ResponseHeaders (IO LBS.ByteString)
resetContent = Response
    { status  = Status.status 205
    , headers = Headers.raw
    , body    = Body.raw
    }

partialContent :: Response S206 Types.ResponseHeaders (IO LBS.ByteString)
partialContent = Response
    { status  = Status.status 206
    , headers = Headers.raw
    , body    = Body.raw
    }

multipleChoices :: Response S300 Types.ResponseHeaders (IO LBS.ByteString)
multipleChoices = Response
    { status  = Status.status 300
    , headers = Headers.raw
    , body    = Body.raw
    }

movedPermanently :: Response S301 Types.ResponseHeaders (IO LBS.ByteString)
movedPermanently = Response
    { status  = Status.status 301
    , headers = Headers.raw
    , body    = Body.raw
    }

found :: Response S302 Types.ResponseHeaders (IO LBS.ByteString)
found = Response
    { status  = Status.status 302
    , headers = Headers.raw
    , body    = Body.raw
    }

seeOther :: Response S303 Types.ResponseHeaders (IO LBS.ByteString)
seeOther = Response
    { status  = Status.status 303
    , headers = Headers.raw
    , body    = Body.raw
    }

notModified :: Response S304 Types.ResponseHeaders (IO LBS.ByteString)
notModified = Response
    { status  = Status.status 304
    , headers = Headers.raw
    , body    = Body.raw
    }

useProxy :: Response S305 Types.ResponseHeaders (IO LBS.ByteString)
useProxy = Response
    { status  = Status.status 305
    , headers = Headers.raw
    , body    = Body.raw
    }

temporaryRedirect :: Response S307 Types.ResponseHeaders (IO LBS.ByteString)
temporaryRedirect = Response
    { status  = Status.status 307
    , headers = Headers.raw
    , body    = Body.raw
    }

permanentRedirect :: Response S308 Types.ResponseHeaders (IO LBS.ByteString)
permanentRedirect = Response
    { status  = Status.status 308
    , headers = Headers.raw
    , body    = Body.raw
    }

badRequest :: Response S400 Types.ResponseHeaders (IO LBS.ByteString)
badRequest = Response
    { status  = Status.status 400
    , headers = Headers.raw
    , body    = Body.raw
    }

unauthorized :: Response S401 Types.ResponseHeaders (IO LBS.ByteString)
unauthorized = Response
    { status  = Status.status 401
    , headers = Headers.raw
    , body    = Body.raw
    }

paymentRequired :: Response S402 Types.ResponseHeaders (IO LBS.ByteString)
paymentRequired = Response
    { status  = Status.status 402
    , headers = Headers.raw
    , body    = Body.raw
    }

forbidden :: Response S403 Types.ResponseHeaders (IO LBS.ByteString)
forbidden = Response
    { status  = Status.status 403
    , headers = Headers.raw
    , body    = Body.raw
    }

notFound :: Response S404 Types.ResponseHeaders (IO LBS.ByteString)
notFound = Response
    { status  = Status.status 404
    , headers = Headers.raw
    , body    = Body.raw
    }

methodNotAllowed :: Response S405 Types.ResponseHeaders (IO LBS.ByteString)
methodNotAllowed = Response
    { status  = Status.status 405
    , headers = Headers.raw
    , body    = Body.raw
    }

notAcceptable :: Response S406 Types.ResponseHeaders (IO LBS.ByteString)
notAcceptable = Response
    { status  = Status.status 406
    , headers = Headers.raw
    , body    = Body.raw
    }

proxyAuthenticationRequired :: Response S407 Types.ResponseHeaders (IO LBS.ByteString)
proxyAuthenticationRequired = Response
    { status  = Status.status 407
    , headers = Headers.raw
    , body    = Body.raw
    }

requestTimeout :: Response S408 Types.ResponseHeaders (IO LBS.ByteString)
requestTimeout = Response
    { status  = Status.status 408
    , headers = Headers.raw
    , body    = Body.raw
    }

conflict :: Response S409 Types.ResponseHeaders (IO LBS.ByteString)
conflict = Response
    { status  = Status.status 409
    , headers = Headers.raw
    , body    = Body.raw
    }

gone :: Response S410 Types.ResponseHeaders (IO LBS.ByteString)
gone = Response
    { status  = Status.status 410
    , headers = Headers.raw
    , body    = Body.raw
    }

lengthRequired :: Response S411 Types.ResponseHeaders (IO LBS.ByteString)
lengthRequired = Response
    { status  = Status.status 411
    , headers = Headers.raw
    , body    = Body.raw
    }

preconditionFailed :: Response S412 Types.ResponseHeaders (IO LBS.ByteString)
preconditionFailed = Response
    { status  = Status.status 412
    , headers = Headers.raw
    , body    = Body.raw
    }

requestEntityTooLarge :: Response S413 Types.ResponseHeaders (IO LBS.ByteString)
requestEntityTooLarge = Response
    { status  = Status.status 413
    , headers = Headers.raw
    , body    = Body.raw
    }

requestURITooLong :: Response S414 Types.ResponseHeaders (IO LBS.ByteString)
requestURITooLong = Response
    { status  = Status.status 414
    , headers = Headers.raw
    , body    = Body.raw
    }

unsupportedMediaType :: Response S415 Types.ResponseHeaders (IO LBS.ByteString)
unsupportedMediaType = Response
    { status  = Status.status 415
    , headers = Headers.raw
    , body    = Body.raw
    }

requestedRangeNotSatisfiable :: Response S416 Types.ResponseHeaders (IO LBS.ByteString)
requestedRangeNotSatisfiable = Response
    { status  = Status.status 416
    , headers = Headers.raw
    , body    = Body.raw
    }

expectationFailed :: Response S417 Types.ResponseHeaders (IO LBS.ByteString)
expectationFailed = Response
    { status  = Status.status 417
    , headers = Headers.raw
    , body    = Body.raw
    }

imATeapot :: Response S418 Types.ResponseHeaders (IO LBS.ByteString)
imATeapot = Response
    { status  = Status.status 418
    , headers = Headers.raw
    , body    = Body.raw
    }

unprocessableEntity :: Response S422 Types.ResponseHeaders (IO LBS.ByteString)
unprocessableEntity = Response
    { status  = Status.status 422
    , headers = Headers.raw
    , body    = Body.raw
    }

preconditionRequired :: Response S428 Types.ResponseHeaders (IO LBS.ByteString)
preconditionRequired = Response
    { status  = Status.status 428
    , headers = Headers.raw
    , body    = Body.raw
    }

tooManyRequests :: Response S429 Types.ResponseHeaders (IO LBS.ByteString)
tooManyRequests = Response
    { status  = Status.status 429
    , headers = Headers.raw
    , body    = Body.raw
    }

requestHeaderFieldsTooLarge :: Response S431 Types.ResponseHeaders (IO LBS.ByteString)
requestHeaderFieldsTooLarge = Response
    { status  = Status.status 431
    , headers = Headers.raw
    , body    = Body.raw
    }

internalServerError :: Response S500 Types.ResponseHeaders (IO LBS.ByteString)
internalServerError = Response
    { status  = Status.status 500
    , headers = Headers.raw
    , body    = Body.raw
    }

notImplemented :: Response S501 Types.ResponseHeaders (IO LBS.ByteString)
notImplemented = Response
    { status  = Status.status 501
    , headers = Headers.raw
    , body    = Body.raw
    }

badGateway :: Response S502 Types.ResponseHeaders (IO LBS.ByteString)
badGateway = Response
    { status  = Status.status 502
    , headers = Headers.raw
    , body    = Body.raw
    }

serviceUnavailable :: Response S503 Types.ResponseHeaders (IO LBS.ByteString)
serviceUnavailable = Response
    { status  = Status.status 503
    , headers = Headers.raw
    , body    = Body.raw
    }

gatewayTimeout :: Response S504 Types.ResponseHeaders (IO LBS.ByteString)
gatewayTimeout = Response
    { status  = Status.status 504
    , headers = Headers.raw
    , body    = Body.raw
    }

httpVersionNotSupported :: Response S505 Types.ResponseHeaders (IO LBS.ByteString)
httpVersionNotSupported = Response
    { status  = Status.status 505
    , headers = Headers.raw
    , body    = Body.raw
    }

networkAuthenticationRequired :: Response S511 Types.ResponseHeaders (IO LBS.ByteString)
networkAuthenticationRequired = Response
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
