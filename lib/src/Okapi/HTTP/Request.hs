{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}

module Okapi.HTTP.Request (
    Request (..),
    any,
    get,
    post,
    put,
    delete,
    patch,
    head,
    options,
    connect,
    trace,
    method,
    path,
    query,
    headers,
    body,
    parser,
    parser',
    printer,

    -- * Side-pinned header combinators (re-exported from "Okapi.HTTP.Request.Headers")
    cookie,
    cookie',

    -- * Side-pinned body combinator (re-exported from "Okapi.HTTP.Request.Body")
    form,

    -- * Method singletons (re-exported from "Okapi.HTTP.Request.Method") -- the full set
    KnownMethod (..),
    GET,
    POST,
    PUT,
    DELETE,
    PATCH,
    HEAD,
    OPTIONS,
    CONNECT,
    TRACE,

    -- * Path combinators (re-exported from "Okapi.HTTP.Request.Path")
    seg,
    seg_,
    lit,
    segs,

    -- * Query combinators (re-exported from "Okapi.HTTP.Request.Query")
    param,
    param',
    param_,
    flag,
    flag',
    list,
    list',
    ArrayStyle (..),
) where

import Prelude hiding (any, head)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Text (Text)
import Network.HTTP.Types qualified as Types
import Network.Wai qualified as Wai
import Okapi.HTTP.Headers qualified as Headers
import Okapi.HTTP.Body qualified as Body
import Okapi.HTTP.Request.Body (RequestBody, form)
import Okapi.HTTP.Request.Headers (RequestHeaders, cookie, cookie', coalesceCookies)
import Okapi.HTTP.Request.Method (CONNECT, DELETE, GET, HEAD, KnownMethod (..), OPTIONS, PATCH, POST, PUT, TRACE)
import Okapi.HTTP.Request.Method qualified as Method
import Okapi.HTTP.Request.Path (Path, seg, seg_, lit, segs)
import Okapi.HTTP.Request.Path qualified as Path
import Okapi.HTTP.Request.Query (Query, ArrayStyle (..), param, param', param_, flag, flag', list, list')
import Okapi.HTTP.Request.Query qualified as Query
import Okapi.HTTP.Tree (SymTree)
import Okapi.Data.Request qualified as Data
import Okapi.Result.Request qualified as Result
import Okapi.Failure.Request qualified as Error

-- | Codecs for every part of an HTTP request.
data Request method path query headers body = Request
    { method  :: Method.Method method
    , path    :: SymTree Path path
    , query   :: SymTree Query query
    , headers :: SymTree RequestHeaders headers
    , body    :: RequestBody body
    }

any :: Request Types.Method [Text] Types.Query Types.RequestHeaders (IO LBS.ByteString)
any =
    Request
        { method = Method.raw
        , path = Path.raw
        , query = Query.raw
        , headers = Headers.raw
        , body = Body.raw
        }

get :: Request GET [Text] Types.Query Types.RequestHeaders (IO LBS.ByteString)
get = any{method = Method.method GET}

post :: Request POST [Text] Types.Query Types.RequestHeaders (IO LBS.ByteString)
post = any{method = Method.method POST}

put :: Request PUT [Text] Types.Query Types.RequestHeaders (IO LBS.ByteString)
put = any{method = Method.method PUT}

delete :: Request DELETE [Text] Types.Query Types.RequestHeaders (IO LBS.ByteString)
delete = any{method = Method.method DELETE}

patch :: Request PATCH [Text] Types.Query Types.RequestHeaders (IO LBS.ByteString)
patch = any{method = Method.method PATCH}

head :: Request HEAD [Text] Types.Query Types.RequestHeaders (IO LBS.ByteString)
head = any{method = Method.method HEAD}

options :: Request OPTIONS [Text] Types.Query Types.RequestHeaders (IO LBS.ByteString)
options = any{method = Method.method OPTIONS}

connect :: Request CONNECT [Text] Types.Query Types.RequestHeaders (IO LBS.ByteString)
connect = any{method = Method.method CONNECT}

trace :: Request TRACE [Text] Types.Query Types.RequestHeaders (IO LBS.ByteString)
trace = any{method = Method.method TRACE}

method ::
    KnownMethod m ->
    Request Types.Method path query headers body ->
    Request (KnownMethod m) path query headers body
method km r = r{method = Method.method km}

path ::
    SymTree Path path ->
    Request method [Text] query headers body ->
    Request method path query headers body
path c r = r{path = c}

query ::
    SymTree Query query ->
    Request method path Types.Query headers body ->
    Request method path query headers body
query c r = r{query = c}

headers ::
    SymTree RequestHeaders headers ->
    Request method path query Types.RequestHeaders body ->
    Request method path query headers body
headers c r = r{headers = c}

body ::
    RequestBody body ->
    Request method path query headers (IO LBS.ByteString) ->
    Request method path query headers body
body c r = r{body = c}

parser ::
    Request method path query headers body ->
    Wai.Request ->
    IO (Either (Error.Request method path query headers body) (Data.Request method path query headers body))
parser req waiReq = do
    let mr = Method.parse req.method (Wai.requestMethod waiReq)
        pr = Path.parseExact req.path (Wai.pathInfo waiReq)
        (qr, _) = Query.parser req.query (Wai.queryString waiReq)
        (hr, _) = Headers.parser req.headers (Wai.requestHeaders waiReq)
        bodyIO = Wai.strictRequestBody waiReq
        br = Body.parser req.body bodyIO
    pure $ case (mr, pr, qr, hr, br) of
        (Right method, Right path, Right query, Right headers, Right body) ->
            Right $ Data.Request{method, path, query, headers, body}
        _ ->
            Left $
                Error.Request
                    { method = either Just (const Nothing) mr
                    , path = either (Just . either id (const Path.ParseError)) (const Nothing) pr
                    , query = either Just (const Nothing) qr
                    , headers = either Just (const Nothing) hr
                    , body = either Just (const Nothing) br
                    }

parser' ::
    Request method path query headers body ->
    Wai.Request ->
    IO (Result.Request method path query headers body)
parser' req waiReq = do
    let mr = Method.parse req.method (Wai.requestMethod waiReq)
        pr = Path.parseExact req.path (Wai.pathInfo waiReq)
        (qr, _) = Query.parser req.query (Wai.queryString waiReq)
        (hr, _) = Headers.parser req.headers (Wai.requestHeaders waiReq)
        bodyIO = Wai.strictRequestBody waiReq
        br = Body.parser req.body bodyIO
    pure $
        Result.Request
            { method = mr
            , path = either (Left . either id (const Path.ParseError)) Right pr
            , query = qr
            , headers = hr
            , body = br
            }

printer ::
    Request method path query headers body ->
    Data.Request method path query headers body ->
    IO Wai.Request
printer req rv = do
    bodyBytes <- Body.printer req.body rv.body
    bodyRef <- newIORef (LBS.toChunks bodyBytes)
    let streamBody = do
            chunks <- readIORef bodyRef
            case chunks of
                [] -> pure BS.empty
                (c : cs) -> writeIORef bodyRef cs >> pure c
    let baseReq =
            Wai.defaultRequest
                { Wai.requestMethod = Method.print req.method rv.method
                , Wai.pathInfo = Path.printer req.path rv.path
                , Wai.queryString = Query.printer req.query rv.query
                , Wai.requestHeaders = coalesceCookies (Headers.printer req.headers rv.headers)
                }
    pure (Wai.setRequestBodyChunks streamBody baseReq)
