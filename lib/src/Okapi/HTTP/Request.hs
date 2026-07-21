{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}

module Okapi.HTTP.Request (
    Request (..),
    Base,
    base,
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
    parser',
    resultToValue,
    resultToError,
    parser,
    printer,

    -- * Side-pinned header combinators (re-exported from "Okapi.HTTP.Headers")
    cookie,
    cookie',

    -- * Side-pinned body combinator (re-exported from "Okapi.HTTP.Body")
    form,

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

import Prelude hiding (head)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.IORef (newIORef, readIORef, writeIORef)
import Network.Wai qualified as Wai
import Okapi.HTTP.Headers (cookie, cookie', coalesceCookies)
import Okapi.HTTP.Headers qualified as Headers
import Okapi.HTTP.Body (form)
import Okapi.HTTP.Body qualified as Body
import Okapi.HTTP.Request.Method qualified as Method
import Okapi.HTTP.Request.Path (Path, seg, seg_, lit, segs)
import Okapi.HTTP.Request.Path qualified as Path
import Okapi.HTTP.Request.Query (Query, ArrayStyle (..), param, param', param_, flag, flag', list, list')
import Okapi.HTTP.Request.Query qualified as Query
import Okapi.HTTP.Tree (SymTree, ForRequest)
import Okapi.Request.Data qualified as Data
import Okapi.Request.Result qualified as Result
import Okapi.Request.Failure qualified as Error

-- | Codecs for every part of an HTTP request.
data Request method path query headers body = Request
    { method  :: Method.Method method
    , path    :: SymTree Path path
    , query   :: SymTree Query query
    , headers :: SymTree (Headers.Headers ForRequest) headers
    , body    :: Body.Body ForRequest body
    }

-- | The maximally unconstrained 'Request' — every slot left raw.
type Base = Request Method.Base Path.Base Query.Base Headers.Base Body.Base

base :: Request Method.Base Path.Base Query.Base Headers.Base Body.Base
base =
    Request
        { method = Method.base
        , path = Path.base
        , query = Query.base
        , headers = Headers.base
        , body = Body.base
        }

get :: Request Method.GET Path.Base Query.Base Headers.Base Body.Base
get = base{method = Method.method Method.Get}

post :: Request Method.POST Path.Base Query.Base Headers.Base Body.Base
post = base{method = Method.method Method.Post}

put :: Request Method.PUT Path.Base Query.Base Headers.Base Body.Base
put = base{method = Method.method Method.Put}

delete :: Request Method.DELETE Path.Base Query.Base Headers.Base Body.Base
delete = base{method = Method.method Method.Delete}

patch :: Request Method.PATCH Path.Base Query.Base Headers.Base Body.Base
patch = base{method = Method.method Method.Patch}

head :: Request Method.HEAD Path.Base Query.Base Headers.Base Body.Base
head = base{method = Method.method Method.Head}

options :: Request Method.OPTIONS Path.Base Query.Base Headers.Base Body.Base
options = base{method = Method.method Method.Options}

connect :: Request Method.CONNECT Path.Base Query.Base Headers.Base Body.Base
connect = base{method = Method.method Method.Connect}

trace :: Request Method.TRACE Path.Base Query.Base Headers.Base Body.Base
trace = base{method = Method.method Method.Trace}

method ::
    Method.KnownMethod m ->
    Request Method.Base path query headers body ->
    Request (Method.KnownMethod m) path query headers body
method km r = r{method = Method.method km}

path ::
    SymTree Path path ->
    Request method Path.Base query headers body ->
    Request method path query headers body
path c r = r{path = c}

query ::
    SymTree Query query ->
    Request method path Query.Base headers body ->
    Request method path query headers body
query c r = r{query = c}

headers ::
    SymTree (Headers.Headers ForRequest) headers ->
    Request method path query Headers.Base body ->
    Request method path query headers body
headers c r = r{headers = c}

body ::
    Body.Body ForRequest body ->
    Request method path query headers Body.Base ->
    Request method path query headers body
body c r = r{body = c}

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
        body = Body.parser req.body bodyIO
    pure $
        Result.Request
            { method = mr
            , path = either (Left . either id (const Path.ParseError)) Right pr
            , query = qr
            , headers = hr
            , body
            }

resultToValue ::
    Result.Request method path query headers body ->
    Maybe (Data.Request method path query headers body)
resultToValue result = case (result.method, result.path, result.query, result.headers) of
    (Right method, Right path, Right query, Right headers) ->
        Just $ Data.Request{method, path, query, headers, body = result.body}
    _ -> Nothing

resultToError ::
    Result.Request method path query headers body ->
    Error.Request method path query headers body
resultToError result =
    Error.Request
        { method = either Just (const Nothing) result.method
        , path = either Just (const Nothing) result.path
        , query = either Just (const Nothing) result.query
        , headers = either Just (const Nothing) result.headers
        }

parser ::
    Request method path query headers body ->
    Wai.Request ->
    IO (Either (Error.Request method path query headers body) (Data.Request method path query headers body))
parser req waiReq = do
    rr <- parser' req waiReq
    pure $ maybe (Left (resultToError rr)) Right (resultToValue rr)

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
