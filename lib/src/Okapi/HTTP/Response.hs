{-# LANGUAGE NoFieldSelectors #-}

module Okapi.HTTP.Response (
    Response (..),
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
    parser,
    printer,

    -- * Side-pinned header combinator (re-exported from "Okapi.HTTP.Headers")
    setCookie,
) where

import Data.ByteString.Builder qualified as Builder
import Data.ByteString.Lazy qualified as LBS
import Network.Wai qualified as Wai
import Network.Wai.Internal qualified as WaiI
import Okapi.Response.Data qualified as Data
import Okapi.Response.Failure qualified as Error
import Okapi.HTTP.Body qualified as Body
import Okapi.HTTP.Headers (setCookie)
import Okapi.HTTP.Headers qualified as Headers
import Okapi.HTTP.Response.Status (
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
import Okapi.HTTP.Response.Status qualified as Status
import Okapi.HTTP.Tree (SymTree, ForResponse)
import Okapi.Response.Result qualified as Result

-- | Codecs for every part of an HTTP response.
data Response status headers body = Response
    { status :: Status.Status status
    , headers :: SymTree (Headers.Headers ForResponse) headers
    , body :: Body.Body ForResponse body
    }

-- | The maximally unconstrained 'Response' — every slot left raw.
type Base = Response Status.Base Headers.Base Body.Base

base :: Base
base =
    Response
        { status = Status.base
        , headers = Headers.base
        , body = Body.base
        }

continue :: Response S100 Headers.Base Body.Base
continue =
    Response
        { status = Status.status 100
        , headers = Headers.base
        , body = Body.base
        }

switchingProtocols :: Response S101 Headers.Base Body.Base
switchingProtocols =
    Response
        { status = Status.status 101
        , headers = Headers.base
        , body = Body.base
        }

ok :: Response S200 Headers.Base Body.Base
ok =
    Response
        { status = Status.status 200
        , headers = Headers.base
        , body = Body.base
        }

created :: Response S201 Headers.Base Body.Base
created =
    Response
        { status = Status.status 201
        , headers = Headers.base
        , body = Body.base
        }

accepted :: Response S202 Headers.Base Body.Base
accepted =
    Response
        { status = Status.status 202
        , headers = Headers.base
        , body = Body.base
        }

nonAuthoritative :: Response S203 Headers.Base Body.Base
nonAuthoritative =
    Response
        { status = Status.status 203
        , headers = Headers.base
        , body = Body.base
        }

noContent :: Response S204 Headers.Base (IO Body.None)
noContent =
    Response
        { status = Status.status 204
        , headers = Headers.base
        , body = Body.none
        }

resetContent :: Response S205 Headers.Base Body.Base
resetContent =
    Response
        { status = Status.status 205
        , headers = Headers.base
        , body = Body.base
        }

partialContent :: Response S206 Headers.Base Body.Base
partialContent =
    Response
        { status = Status.status 206
        , headers = Headers.base
        , body = Body.base
        }

multipleChoices :: Response S300 Headers.Base Body.Base
multipleChoices =
    Response
        { status = Status.status 300
        , headers = Headers.base
        , body = Body.base
        }

movedPermanently :: Response S301 Headers.Base Body.Base
movedPermanently =
    Response
        { status = Status.status 301
        , headers = Headers.base
        , body = Body.base
        }

found :: Response S302 Headers.Base Body.Base
found =
    Response
        { status = Status.status 302
        , headers = Headers.base
        , body = Body.base
        }

seeOther :: Response S303 Headers.Base Body.Base
seeOther =
    Response
        { status = Status.status 303
        , headers = Headers.base
        , body = Body.base
        }

notModified :: Response S304 Headers.Base Body.Base
notModified =
    Response
        { status = Status.status 304
        , headers = Headers.base
        , body = Body.base
        }

useProxy :: Response S305 Headers.Base Body.Base
useProxy =
    Response
        { status = Status.status 305
        , headers = Headers.base
        , body = Body.base
        }

temporaryRedirect :: Response S307 Headers.Base Body.Base
temporaryRedirect =
    Response
        { status = Status.status 307
        , headers = Headers.base
        , body = Body.base
        }

permanentRedirect :: Response S308 Headers.Base Body.Base
permanentRedirect =
    Response
        { status = Status.status 308
        , headers = Headers.base
        , body = Body.base
        }

badRequest :: Response S400 Headers.Base Body.Base
badRequest =
    Response
        { status = Status.status 400
        , headers = Headers.base
        , body = Body.base
        }

unauthorized :: Response S401 Headers.Base Body.Base
unauthorized =
    Response
        { status = Status.status 401
        , headers = Headers.base
        , body = Body.base
        }

paymentRequired :: Response S402 Headers.Base Body.Base
paymentRequired =
    Response
        { status = Status.status 402
        , headers = Headers.base
        , body = Body.base
        }

forbidden :: Response S403 Headers.Base Body.Base
forbidden =
    Response
        { status = Status.status 403
        , headers = Headers.base
        , body = Body.base
        }

notFound :: Response S404 Headers.Base Body.Base
notFound =
    Response
        { status = Status.status 404
        , headers = Headers.base
        , body = Body.base
        }

methodNotAllowed :: Response S405 Headers.Base Body.Base
methodNotAllowed =
    Response
        { status = Status.status 405
        , headers = Headers.base
        , body = Body.base
        }

notAcceptable :: Response S406 Headers.Base Body.Base
notAcceptable =
    Response
        { status = Status.status 406
        , headers = Headers.base
        , body = Body.base
        }

proxyAuthenticationRequired :: Response S407 Headers.Base Body.Base
proxyAuthenticationRequired =
    Response
        { status = Status.status 407
        , headers = Headers.base
        , body = Body.base
        }

requestTimeout :: Response S408 Headers.Base Body.Base
requestTimeout =
    Response
        { status = Status.status 408
        , headers = Headers.base
        , body = Body.base
        }

conflict :: Response S409 Headers.Base Body.Base
conflict =
    Response
        { status = Status.status 409
        , headers = Headers.base
        , body = Body.base
        }

gone :: Response S410 Headers.Base Body.Base
gone =
    Response
        { status = Status.status 410
        , headers = Headers.base
        , body = Body.base
        }

lengthRequired :: Response S411 Headers.Base Body.Base
lengthRequired =
    Response
        { status = Status.status 411
        , headers = Headers.base
        , body = Body.base
        }

preconditionFailed :: Response S412 Headers.Base Body.Base
preconditionFailed =
    Response
        { status = Status.status 412
        , headers = Headers.base
        , body = Body.base
        }

requestEntityTooLarge :: Response S413 Headers.Base Body.Base
requestEntityTooLarge =
    Response
        { status = Status.status 413
        , headers = Headers.base
        , body = Body.base
        }

requestURITooLong :: Response S414 Headers.Base Body.Base
requestURITooLong =
    Response
        { status = Status.status 414
        , headers = Headers.base
        , body = Body.base
        }

unsupportedMediaType :: Response S415 Headers.Base Body.Base
unsupportedMediaType =
    Response
        { status = Status.status 415
        , headers = Headers.base
        , body = Body.base
        }

requestedRangeNotSatisfiable :: Response S416 Headers.Base Body.Base
requestedRangeNotSatisfiable =
    Response
        { status = Status.status 416
        , headers = Headers.base
        , body = Body.base
        }

expectationFailed :: Response S417 Headers.Base Body.Base
expectationFailed =
    Response
        { status = Status.status 417
        , headers = Headers.base
        , body = Body.base
        }

imATeapot :: Response S418 Headers.Base Body.Base
imATeapot =
    Response
        { status = Status.status 418
        , headers = Headers.base
        , body = Body.base
        }

unprocessableEntity :: Response S422 Headers.Base Body.Base
unprocessableEntity =
    Response
        { status = Status.status 422
        , headers = Headers.base
        , body = Body.base
        }

preconditionRequired :: Response S428 Headers.Base Body.Base
preconditionRequired =
    Response
        { status = Status.status 428
        , headers = Headers.base
        , body = Body.base
        }

tooManyRequests :: Response S429 Headers.Base Body.Base
tooManyRequests =
    Response
        { status = Status.status 429
        , headers = Headers.base
        , body = Body.base
        }

requestHeaderFieldsTooLarge :: Response S431 Headers.Base Body.Base
requestHeaderFieldsTooLarge =
    Response
        { status = Status.status 431
        , headers = Headers.base
        , body = Body.base
        }

internalServerError :: Response S500 Headers.Base Body.Base
internalServerError =
    Response
        { status = Status.status 500
        , headers = Headers.base
        , body = Body.base
        }

notImplemented :: Response S501 Headers.Base Body.Base
notImplemented =
    Response
        { status = Status.status 501
        , headers = Headers.base
        , body = Body.base
        }

badGateway :: Response S502 Headers.Base Body.Base
badGateway =
    Response
        { status = Status.status 502
        , headers = Headers.base
        , body = Body.base
        }

serviceUnavailable :: Response S503 Headers.Base Body.Base
serviceUnavailable =
    Response
        { status = Status.status 503
        , headers = Headers.base
        , body = Body.base
        }

gatewayTimeout :: Response S504 Headers.Base Body.Base
gatewayTimeout =
    Response
        { status = Status.status 504
        , headers = Headers.base
        , body = Body.base
        }

httpVersionNotSupported :: Response S505 Headers.Base Body.Base
httpVersionNotSupported =
    Response
        { status = Status.status 505
        , headers = Headers.base
        , body = Body.base
        }

networkAuthenticationRequired :: Response S511 Headers.Base Body.Base
networkAuthenticationRequired =
    Response
        { status = Status.status 511
        , headers = Headers.base
        , body = Body.base
        }

headers ::
    SymTree (Headers.Headers ForResponse) headers ->
    Response status Headers.Base body ->
    Response status headers body
headers c r = r{headers = c}

body ::
    Body.Body ForResponse body ->
    Response status headers Body.Base ->
    Response status headers body
body c r = r{body = c}

extractWaiResBody :: Wai.Response -> IO LBS.ByteString
extractWaiResBody (WaiI.ResponseBuilder _ _ b) = pure (Builder.toLazyByteString b)
extractWaiResBody _ = pure LBS.empty

parser' ::
    Response status headers body ->
    Wai.Response ->
    IO (Result.Response status headers body)
parser' codec waiRes = do
    let httpStatus = Wai.responseStatus waiRes
        waiHeaders = Wai.responseHeaders waiRes
        sr = Status.parse codec.status httpStatus
        (hr, _) = Headers.parser codec.headers waiHeaders
        br = Body.parser codec.body (extractWaiResBody waiRes)
    pure $ Result.Response{status = sr, headers = hr, body = br}

resultToValue ::
    Result.Response status headers body ->
    Maybe (Data.Response status headers body)
resultToValue result = case (result.status, result.headers) of
    (Right status, Right headers) ->
        Just $ Data.Response{status = status, headers = headers, body = result.body}
    _ -> Nothing

resultToError ::
    Result.Response status headers body ->
    Error.Response status headers body
resultToError result =
    Error.Response
        { status = either Just (const Nothing) result.status
        , headers = either Just (const Nothing) result.headers
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
