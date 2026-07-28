{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}

module Okapi.HTTP.Response (
    Codec (..),
    Base,
    base,
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
    Data (..),
    Failure (..),
    Result (..),
    parser,
    printer,

    -- * Side-pinned header combinator (re-exported from "Okapi.HTTP.Headers")
    setCookie,
) where

import Data.ByteString.Builder qualified as Builder
import Data.ByteString.Lazy qualified as LBS
import Network.Wai qualified as Wai
import Network.Wai.Internal qualified as WaiI
import Okapi.HTTP.Body qualified as Body
import Okapi.HTTP.Headers (setCookie)
import Okapi.HTTP.Headers (Headers)
import Okapi.HTTP.Headers qualified as Headers
import Okapi.HTTP.Status (
    S100,
    S101,
    S200,
    S201,
    S202,
    S203,
    S204,
    S205,
    S206,
    S300,
    S301,
    S302,
    S303,
    S304,
    S305,
    S307,
    S308,
    S400,
    S401,
    S402,
    S403,
    S404,
    S405,
    S406,
    S407,
    S408,
    S409,
    S410,
    S411,
    S412,
    S413,
    S414,
    S415,
    S416,
    S417,
    S418,
    S422,
    S428,
    S429,
    S431,
    S500,
    S501,
    S502,
    S503,
    S504,
    S505,
    S511
 )
import Okapi.HTTP.Status qualified as Status
import Okapi.Tree (SymTree, ForResponse)
import Okapi.Tree qualified as Tree

-- | Codecs for every part of an HTTP response.
data Codec status headers body = Codec
    { status :: Status.Status status
    , headers :: SymTree (Headers.Headers ForResponse) headers
    , body :: Body.Body ForResponse body
    }

-- | The maximally unconstrained 'Codec' — every slot left raw.
type Base = Codec Status.Base Headers.Base Body.Base

base :: Base
base =
    Codec
        { status = Status.base
        , headers = Headers.base
        , body = Body.base
        }

continue :: Codec S100 Headers.Base Body.Base
continue =
    Codec
        { status = Status.status 100
        , headers = Headers.base
        , body = Body.base
        }

switchingProtocols :: Codec S101 Headers.Base Body.Base
switchingProtocols =
    Codec
        { status = Status.status 101
        , headers = Headers.base
        , body = Body.base
        }

ok :: Codec S200 Headers.Base Body.Base
ok =
    Codec
        { status = Status.status 200
        , headers = Headers.base
        , body = Body.base
        }

created :: Codec S201 Headers.Base Body.Base
created =
    Codec
        { status = Status.status 201
        , headers = Headers.base
        , body = Body.base
        }

accepted :: Codec S202 Headers.Base Body.Base
accepted =
    Codec
        { status = Status.status 202
        , headers = Headers.base
        , body = Body.base
        }

nonAuthoritative :: Codec S203 Headers.Base Body.Base
nonAuthoritative =
    Codec
        { status = Status.status 203
        , headers = Headers.base
        , body = Body.base
        }

noContent :: Codec S204 Headers.Base (IO Body.None)
noContent =
    Codec
        { status = Status.status 204
        , headers = Headers.base
        , body = Body.none
        }

resetContent :: Codec S205 Headers.Base Body.Base
resetContent =
    Codec
        { status = Status.status 205
        , headers = Headers.base
        , body = Body.base
        }

partialContent :: Codec S206 Headers.Base Body.Base
partialContent =
    Codec
        { status = Status.status 206
        , headers = Headers.base
        , body = Body.base
        }

multipleChoices :: Codec S300 Headers.Base Body.Base
multipleChoices =
    Codec
        { status = Status.status 300
        , headers = Headers.base
        , body = Body.base
        }

movedPermanently :: Codec S301 Headers.Base Body.Base
movedPermanently =
    Codec
        { status = Status.status 301
        , headers = Headers.base
        , body = Body.base
        }

found :: Codec S302 Headers.Base Body.Base
found =
    Codec
        { status = Status.status 302
        , headers = Headers.base
        , body = Body.base
        }

seeOther :: Codec S303 Headers.Base Body.Base
seeOther =
    Codec
        { status = Status.status 303
        , headers = Headers.base
        , body = Body.base
        }

notModified :: Codec S304 Headers.Base Body.Base
notModified =
    Codec
        { status = Status.status 304
        , headers = Headers.base
        , body = Body.base
        }

useProxy :: Codec S305 Headers.Base Body.Base
useProxy =
    Codec
        { status = Status.status 305
        , headers = Headers.base
        , body = Body.base
        }

temporaryRedirect :: Codec S307 Headers.Base Body.Base
temporaryRedirect =
    Codec
        { status = Status.status 307
        , headers = Headers.base
        , body = Body.base
        }

permanentRedirect :: Codec S308 Headers.Base Body.Base
permanentRedirect =
    Codec
        { status = Status.status 308
        , headers = Headers.base
        , body = Body.base
        }

badRequest :: Codec S400 Headers.Base Body.Base
badRequest =
    Codec
        { status = Status.status 400
        , headers = Headers.base
        , body = Body.base
        }

unauthorized :: Codec S401 Headers.Base Body.Base
unauthorized =
    Codec
        { status = Status.status 401
        , headers = Headers.base
        , body = Body.base
        }

paymentRequired :: Codec S402 Headers.Base Body.Base
paymentRequired =
    Codec
        { status = Status.status 402
        , headers = Headers.base
        , body = Body.base
        }

forbidden :: Codec S403 Headers.Base Body.Base
forbidden =
    Codec
        { status = Status.status 403
        , headers = Headers.base
        , body = Body.base
        }

notFound :: Codec S404 Headers.Base Body.Base
notFound =
    Codec
        { status = Status.status 404
        , headers = Headers.base
        , body = Body.base
        }

methodNotAllowed :: Codec S405 Headers.Base Body.Base
methodNotAllowed =
    Codec
        { status = Status.status 405
        , headers = Headers.base
        , body = Body.base
        }

notAcceptable :: Codec S406 Headers.Base Body.Base
notAcceptable =
    Codec
        { status = Status.status 406
        , headers = Headers.base
        , body = Body.base
        }

proxyAuthenticationRequired :: Codec S407 Headers.Base Body.Base
proxyAuthenticationRequired =
    Codec
        { status = Status.status 407
        , headers = Headers.base
        , body = Body.base
        }

requestTimeout :: Codec S408 Headers.Base Body.Base
requestTimeout =
    Codec
        { status = Status.status 408
        , headers = Headers.base
        , body = Body.base
        }

conflict :: Codec S409 Headers.Base Body.Base
conflict =
    Codec
        { status = Status.status 409
        , headers = Headers.base
        , body = Body.base
        }

gone :: Codec S410 Headers.Base Body.Base
gone =
    Codec
        { status = Status.status 410
        , headers = Headers.base
        , body = Body.base
        }

lengthRequired :: Codec S411 Headers.Base Body.Base
lengthRequired =
    Codec
        { status = Status.status 411
        , headers = Headers.base
        , body = Body.base
        }

preconditionFailed :: Codec S412 Headers.Base Body.Base
preconditionFailed =
    Codec
        { status = Status.status 412
        , headers = Headers.base
        , body = Body.base
        }

requestEntityTooLarge :: Codec S413 Headers.Base Body.Base
requestEntityTooLarge =
    Codec
        { status = Status.status 413
        , headers = Headers.base
        , body = Body.base
        }

requestURITooLong :: Codec S414 Headers.Base Body.Base
requestURITooLong =
    Codec
        { status = Status.status 414
        , headers = Headers.base
        , body = Body.base
        }

unsupportedMediaType :: Codec S415 Headers.Base Body.Base
unsupportedMediaType =
    Codec
        { status = Status.status 415
        , headers = Headers.base
        , body = Body.base
        }

requestedRangeNotSatisfiable :: Codec S416 Headers.Base Body.Base
requestedRangeNotSatisfiable =
    Codec
        { status = Status.status 416
        , headers = Headers.base
        , body = Body.base
        }

expectationFailed :: Codec S417 Headers.Base Body.Base
expectationFailed =
    Codec
        { status = Status.status 417
        , headers = Headers.base
        , body = Body.base
        }

imATeapot :: Codec S418 Headers.Base Body.Base
imATeapot =
    Codec
        { status = Status.status 418
        , headers = Headers.base
        , body = Body.base
        }

unprocessableEntity :: Codec S422 Headers.Base Body.Base
unprocessableEntity =
    Codec
        { status = Status.status 422
        , headers = Headers.base
        , body = Body.base
        }

preconditionRequired :: Codec S428 Headers.Base Body.Base
preconditionRequired =
    Codec
        { status = Status.status 428
        , headers = Headers.base
        , body = Body.base
        }

tooManyRequests :: Codec S429 Headers.Base Body.Base
tooManyRequests =
    Codec
        { status = Status.status 429
        , headers = Headers.base
        , body = Body.base
        }

requestHeaderFieldsTooLarge :: Codec S431 Headers.Base Body.Base
requestHeaderFieldsTooLarge =
    Codec
        { status = Status.status 431
        , headers = Headers.base
        , body = Body.base
        }

internalServerError :: Codec S500 Headers.Base Body.Base
internalServerError =
    Codec
        { status = Status.status 500
        , headers = Headers.base
        , body = Body.base
        }

notImplemented :: Codec S501 Headers.Base Body.Base
notImplemented =
    Codec
        { status = Status.status 501
        , headers = Headers.base
        , body = Body.base
        }

badGateway :: Codec S502 Headers.Base Body.Base
badGateway =
    Codec
        { status = Status.status 502
        , headers = Headers.base
        , body = Body.base
        }

serviceUnavailable :: Codec S503 Headers.Base Body.Base
serviceUnavailable =
    Codec
        { status = Status.status 503
        , headers = Headers.base
        , body = Body.base
        }

gatewayTimeout :: Codec S504 Headers.Base Body.Base
gatewayTimeout =
    Codec
        { status = Status.status 504
        , headers = Headers.base
        , body = Body.base
        }

httpVersionNotSupported :: Codec S505 Headers.Base Body.Base
httpVersionNotSupported =
    Codec
        { status = Status.status 505
        , headers = Headers.base
        , body = Body.base
        }

networkAuthenticationRequired :: Codec S511 Headers.Base Body.Base
networkAuthenticationRequired =
    Codec
        { status = Status.status 511
        , headers = Headers.base
        , body = Body.base
        }

headers ::
    SymTree (Headers.Headers ForResponse) headers ->
    Codec status Headers.Base body ->
    Codec status headers body
headers c r = r{headers = c}

body ::
    Body.Body ForResponse body ->
    Codec status headers Body.Base ->
    Codec status headers body
body c r = r{body = c}

-- | The decoded value of every part of an HTTP response, once parsing has
--   fully succeeded.
data Data status headers body = Data
    { status  :: status
    , headers :: headers
    , body    :: body
    }

-- | Per-part accumulated parse failures — 'Nothing' where that part parsed
--   fine.
data Failure status headers body = Failure
    { status  :: Maybe Status.ParseError
    , headers :: Maybe (Tree.Failure (Headers ForResponse))
    }

-- | Per-part parse result, before it's known whether every part succeeded.
data Result status headers body = Result
    { status  :: Either Status.ParseError status
    , headers :: Either (Tree.Failure (Headers ForResponse)) headers
    , body    :: body
    }

extractWaiResBody :: Wai.Response -> IO LBS.ByteString
extractWaiResBody (WaiI.ResponseBuilder _ _ b) = pure (Builder.toLazyByteString b)
extractWaiResBody _ = pure LBS.empty

parser' ::
    Codec status headers body ->
    Wai.Response ->
    IO (Result status headers body)
parser' codec waiRes = do
    let httpStatus = Wai.responseStatus waiRes
        waiHeaders = Wai.responseHeaders waiRes
        sr = Status.parse codec.status httpStatus
        (hr, _) = Headers.parser codec.headers waiHeaders
        br = Body.parser codec.body (extractWaiResBody waiRes)
    pure $ Result{status = sr, headers = hr, body = br}

resultToValue ::
    Result status headers body ->
    Maybe (Data status headers body)
resultToValue result = case (result.status, result.headers) of
    (Right status, Right headers) ->
        Just $ Data{status = status, headers = headers, body = result.body}
    _ -> Nothing

resultToError ::
    Result status headers body ->
    Failure status headers body
resultToError result =
    Failure
        { status = either Just (const Nothing) result.status
        , headers = either Just (const Nothing) result.headers
        }

parser ::
    Codec status headers body ->
    Wai.Response ->
    IO (Either (Failure status headers body) (Data status headers body))
parser res waiRes = do
    rr <- parser' res waiRes
    pure $ maybe (Left (resultToError rr)) Right (resultToValue rr)

printer ::
    Codec status headers body ->
    Data status headers body ->
    IO Wai.Response
printer codec value = do
    bodyBytes <- Body.printer codec.body value.body
    let responseHeaders = Headers.printer codec.headers value.headers
    pure $ Wai.responseLBS (Status.print codec.status value.status) responseHeaders bodyBytes
