{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Okapi.HTTP.Request (
    request,
    methodGET,
    methodPOST,
    methodPUT,
    methodDELETE,
    method,
    path,
    query,
    headers,
    body,
    pathOf,
    queryOf,
    headersOf,
    parseRequest,
    parseRequestResult,
    printRequest,
    linkTo,
) where

import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8)
import GHC.Generics (Generic, Rep)
import Network.HTTP.Types qualified as HTTP
import Network.Wai qualified as Wai
import Okapi.Mode.Tree (Request (..))
import Okapi.Mode.Tree qualified as Tree
import Okapi.Mode.Error    qualified as Error
import Okapi.Mode.Result   qualified as Result
import Okapi.Mode.Data    qualified as Data
import Okapi.HTTP.Request.Body (Body)
import Okapi.HTTP.Request.Body qualified as Body
import Okapi.HTTP.Request.Headers (GHeaders, Headers)
import Okapi.HTTP.Request.Headers qualified as Headers
import Okapi.HTTP.Request.Method (DELETE, GET, KnownMethod (..), Method, POST, PUT)
import Okapi.HTTP.Request.Method qualified as Method
import Okapi.HTTP.Request.Path (GPath, Path)
import Okapi.HTTP.Request.Path qualified as Path
import Okapi.HTTP.Request.Query (GQuery, Query)
import Okapi.HTTP.Request.Query qualified as Query
import Okapi.Tree (SymTree)

request :: Tree.Request HTTP.Method [Text] HTTP.Query HTTP.RequestHeaders (IO LBS.ByteString)
request = Request
    { method  = Method.raw
    , path    = Path.raw
    , query   = Query.raw
    , headers = Headers.raw
    , body    = Body.raw
    }

methodGET :: Tree.Request GET [Text] HTTP.Query HTTP.RequestHeaders (IO LBS.ByteString)
methodGET = request { method = Method.method GET }

methodPOST :: Tree.Request POST [Text] HTTP.Query HTTP.RequestHeaders (IO LBS.ByteString)
methodPOST = request { method = Method.method POST }

methodPUT :: Tree.Request PUT [Text] HTTP.Query HTTP.RequestHeaders (IO LBS.ByteString)
methodPUT = request { method = Method.method PUT }

methodDELETE :: Tree.Request DELETE [Text] HTTP.Query HTTP.RequestHeaders (IO LBS.ByteString)
methodDELETE = request { method = Method.method DELETE }

method ::
    KnownMethod m ->
    Tree.Request HTTP.Method path query headers body ->
    Tree.Request (KnownMethod m) path query headers body
method km r = r { method = Method.method km }

path ::
    SymTree Path path ->
    Tree.Request method [Text] query headers body ->
    Tree.Request method path query headers body
path c r = r { path = c }

query ::
    SymTree Query query ->
    Tree.Request method path HTTP.Query headers body ->
    Tree.Request method path query headers body
query c r = r { query = c }

headers ::
    SymTree Headers headers ->
    Tree.Request method path query HTTP.RequestHeaders body ->
    Tree.Request method path query headers body
headers c r = r { headers = c }

body ::
    SymTree Body body ->
    Tree.Request method path query headers (IO LBS.ByteString) ->
    Tree.Request method path query headers body
body c r = r { body = c }

pathOf ::
    forall path method query headers body.
    (Generic path, GPath (Rep path)) =>
    Tree.Request method [Text] query headers body ->
    Tree.Request method path query headers body
pathOf r = r { path = Path.pathCodec @path }

queryOf ::
    forall query method path headers body.
    (Generic query, GQuery (Rep query)) =>
    Tree.Request method path HTTP.Query headers body ->
    Tree.Request method path query headers body
queryOf r = r { query = Query.queryCodec @query }

headersOf ::
    forall headers method path query body.
    (Generic headers, GHeaders (Rep headers)) =>
    Tree.Request method path query HTTP.RequestHeaders body ->
    Tree.Request method path query headers body
headersOf r = r { headers = Headers.headersCodec @headers }

parseRequest ::
    Tree.Request method path query headers body ->
    Wai.Request ->
    IO (Either (Error.Request method path query headers body) (Data.Request method path query headers body))
parseRequest req waiReq = do
    let mr      = Method.parse    req.method  (Wai.requestMethod  waiReq)
        pr      = Path.parseExact req.path    (Wai.pathInfo       waiReq)
        (qr, _) = Query.parse    req.query   (Wai.queryString    waiReq)
        (hr, _) = Headers.parse  req.headers (Wai.requestHeaders waiReq)
        bodyIO  = Wai.strictRequestBody waiReq
        (br, _) = Body.parse     req.body    bodyIO
    pure $ case (mr, pr, qr, hr, br) of
        (Right method, Right path, Right query, Right headers, Right body) ->
            Right $ Data.Request { method, path, query, headers, body }
        _ ->
            Left $ Error.Request
                { method  = either Just         (const Nothing) mr
                , path    = either (Just . fst) (const Nothing) pr
                , query   = either Just         (const Nothing) qr
                , headers = either Just         (const Nothing) hr
                , body    = either Just         (const Nothing) br
                }

parseRequestResult ::
    Tree.Request method path query headers body ->
    Wai.Request ->
    IO (Result.Request method path query headers body)
parseRequestResult req waiReq = do
    let mr      = Method.parse    req.method  (Wai.requestMethod  waiReq)
        pr      = Path.parseExact req.path    (Wai.pathInfo       waiReq)
        (qr, _) = Query.parse    req.query   (Wai.queryString    waiReq)
        (hr, _) = Headers.parse  req.headers (Wai.requestHeaders waiReq)
        bodyIO  = Wai.strictRequestBody waiReq
        (br, _) = Body.parse     req.body    bodyIO
    pure $ Result.Request
        { method  = mr
        , path    = either (Left . fst) Right pr
        , query   = qr
        , headers = hr
        , body    = br
        }

printRequest ::
    Tree.Request method path query headers body ->
    Data.Request method path query headers body ->
    IO Wai.Request
printRequest req rv = do
    bodyBytes <- Body.print req.body rv.body
    bodyRef   <- newIORef (LBS.toChunks bodyBytes)
    let streamBody = do
            chunks <- readIORef bodyRef
            case chunks of
                []     -> pure BS.empty
                (c:cs) -> writeIORef bodyRef cs >> pure c
    let baseReq = Wai.defaultRequest
            { Wai.requestMethod  = Method.print  req.method  rv.method
            , Wai.pathInfo       = Path.print    req.path    rv.path
            , Wai.queryString    = Query.print   req.query   rv.query
            , Wai.requestHeaders = Headers.print req.headers rv.headers
            }
    pure (Wai.setRequestBodyChunks streamBody baseReq)

linkTo ::
    Tree.Request method path query headers body ->
    Data.Request method path query headers body ->
    Text
linkTo req rv =
    let segs = Path.print  req.path  rv.path
        qs   = Query.print req.query rv.query
    in "/" <> T.intercalate "/" segs <> decodeUtf8 (HTTP.renderQuery True qs)
