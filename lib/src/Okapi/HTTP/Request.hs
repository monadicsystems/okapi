{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}

module Okapi.HTTP.Request (
    Codec (..),
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
    Data (..),
    Failure (..),
    Result (..),
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

    -- * Path combinators (re-exported from "Okapi.HTTP.Path")
    seg,
    seg_,
    lit,
    segs,

    -- * Query combinators (re-exported from "Okapi.HTTP.Query")
    param,
    param',
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
import Okapi.HTTP.Headers (Headers)
import Okapi.HTTP.Headers qualified as Headers
import Okapi.HTTP.Body (form)
import Okapi.HTTP.Body qualified as Body
import Okapi.HTTP.Method qualified as Method
import Okapi.HTTP.Path (Path, seg, seg_, lit, segs)
import Okapi.HTTP.Path qualified as Path
import Okapi.HTTP.Query (Query, ArrayStyle (..), param, param', flag, flag', list, list')
import Okapi.HTTP.Query qualified as Query
import Okapi.Tree (SymTree, ForRequest)
import Okapi.Tree qualified as Tree

-- | Codecs for every part of an HTTP request.
data Codec method path query headers body = Codec
    { method  :: Method.Method method
    , path    :: SymTree Path path
    , query   :: SymTree Query query
    , headers :: SymTree (Headers.Headers ForRequest) headers
    , body    :: Body.Body ForRequest body
    }

-- | The maximally unconstrained 'Codec' — every slot left raw.
type Base = Codec Method.Base Path.Base Query.Base Headers.Base Body.Base

base :: Codec Method.Base Path.Base Query.Base Headers.Base Body.Base
base =
    Codec
        { method = Method.base
        , path = Path.base
        , query = Query.base
        , headers = Headers.base
        , body = Body.base
        }

get :: Codec Method.GET Path.Base Query.Base Headers.Base Body.Base
get = base{method = Method.method Method.Get}

post :: Codec Method.POST Path.Base Query.Base Headers.Base Body.Base
post = base{method = Method.method Method.Post}

put :: Codec Method.PUT Path.Base Query.Base Headers.Base Body.Base
put = base{method = Method.method Method.Put}

delete :: Codec Method.DELETE Path.Base Query.Base Headers.Base Body.Base
delete = base{method = Method.method Method.Delete}

patch :: Codec Method.PATCH Path.Base Query.Base Headers.Base Body.Base
patch = base{method = Method.method Method.Patch}

head :: Codec Method.HEAD Path.Base Query.Base Headers.Base Body.Base
head = base{method = Method.method Method.Head}

options :: Codec Method.OPTIONS Path.Base Query.Base Headers.Base Body.Base
options = base{method = Method.method Method.Options}

connect :: Codec Method.CONNECT Path.Base Query.Base Headers.Base Body.Base
connect = base{method = Method.method Method.Connect}

trace :: Codec Method.TRACE Path.Base Query.Base Headers.Base Body.Base
trace = base{method = Method.method Method.Trace}

method ::
    Method.KnownMethod m ->
    Codec Method.Base path query headers body ->
    Codec (Method.KnownMethod m) path query headers body
method km r = r{method = Method.method km}

path ::
    SymTree Path path ->
    Codec method Path.Base query headers body ->
    Codec method path query headers body
path c r = r{path = c}

query ::
    SymTree Query query ->
    Codec method path Query.Base headers body ->
    Codec method path query headers body
query c r = r{query = c}

headers ::
    SymTree (Headers.Headers ForRequest) headers ->
    Codec method path query Headers.Base body ->
    Codec method path query headers body
headers c r = r{headers = c}

body ::
    Body.Body ForRequest body ->
    Codec method path query headers Body.Base ->
    Codec method path query headers body
body c r = r{body = c}

-- | The decoded value of every part of an HTTP request, once parsing has
--   fully succeeded.
data Data method path query headers body = Data
    { method  :: method
    , path    :: path
    , query   :: query
    , headers :: headers
    , body    :: body
    }

-- | Per-part accumulated parse failures — 'Nothing' where that part parsed
--   fine.
data Failure method path query headers body = Failure
    { method  :: Maybe Method.ParseError
    , path    :: Maybe (Tree.Failure Path)
    , query   :: Maybe (Tree.Failure Query)
    , headers :: Maybe (Tree.Failure (Headers ForRequest))
    }

-- | Per-part parse result, before it's known whether every part succeeded.
data Result method path query headers body = Result
    { method  :: Either Method.ParseError method
    , path    :: Either (Tree.Failure Path) path
    , query   :: Either (Tree.Failure Query) query
    , headers :: Either (Tree.Failure (Headers ForRequest)) headers
    , body    :: body
    }

parser' ::
    Codec method path query headers body ->
    Wai.Request ->
    IO (Result method path query headers body)
parser' req waiReq = do
    let mr = Method.parse req.method (Wai.requestMethod waiReq)
        pr = Path.parseExact req.path (Wai.pathInfo waiReq)
        (qr, _) = Query.parser req.query (Wai.queryString waiReq)
        (hr, _) = Headers.parser req.headers (Wai.requestHeaders waiReq)
        bodyIO = Wai.strictRequestBody waiReq
        body = Body.parser req.body bodyIO
    pure $
        Result
            { method = mr
            , path = either (Left . either id (const Path.ParseError)) Right pr
            , query = qr
            , headers = hr
            , body
            }

resultToValue ::
    Result method path query headers body ->
    Maybe (Data method path query headers body)
resultToValue result = case (result.method, result.path, result.query, result.headers) of
    (Right method, Right path, Right query, Right headers) ->
        Just $ Data{method, path, query, headers, body = result.body}
    _ -> Nothing

resultToError ::
    Result method path query headers body ->
    Failure method path query headers body
resultToError result =
    Failure
        { method = either Just (const Nothing) result.method
        , path = either Just (const Nothing) result.path
        , query = either Just (const Nothing) result.query
        , headers = either Just (const Nothing) result.headers
        }

parser ::
    Codec method path query headers body ->
    Wai.Request ->
    IO (Either (Failure method path query headers body) (Data method path query headers body))
parser req waiReq = do
    rr <- parser' req waiReq
    pure $ maybe (Left (resultToError rr)) Right (resultToValue rr)

printer ::
    Codec method path query headers body ->
    Data method path query headers body ->
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
