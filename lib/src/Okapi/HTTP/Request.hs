{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}

module Okapi.HTTP.Request (
    request,
    requestGET,
    requestPOST,
    requestPUT,
    requestDELETE,
    requestPATCH,
    requestHEAD,
    requestOPTIONS,
    requestCONNECT,
    requestTRACE,
    method,
    path,
    query,
    headers,
    body,
    parser,
    parser',
    printer,
) where

import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Text (Text)
import Network.HTTP.Types qualified as HTTP
import Network.Wai qualified as Wai
import Okapi.HTTP.Request.Body (Body)
import Okapi.HTTP.Request.Body qualified as Body
import Okapi.HTTP.Request.Headers (Headers)
import Okapi.HTTP.Request.Headers qualified as Headers
import Okapi.HTTP.Request.Method (CONNECT, DELETE, GET, HEAD, KnownMethod (..), OPTIONS, PATCH, POST, PUT, TRACE)
import Okapi.HTTP.Request.Method qualified as Method
import Okapi.HTTP.Request.Path (Path)
import Okapi.HTTP.Request.Path qualified as Path
import Okapi.HTTP.Request.Query (Query)
import Okapi.HTTP.Request.Query qualified as Query
import Okapi.Record.Data qualified as Data
import Okapi.Record.Failure qualified as Error
import Okapi.Record.Result qualified as Result
import Okapi.Record.Tree (Request (..))
import Okapi.Record.Tree qualified as Tree
import Okapi.Tree (SymTree)

request :: Tree.Request HTTP.Method [Text] HTTP.Query HTTP.RequestHeaders (IO LBS.ByteString)
request =
    Request
        { method = Method.raw
        , path = Path.raw
        , query = Query.raw
        , headers = Headers.raw
        , body = Body.raw
        }

requestGET :: Tree.Request GET [Text] HTTP.Query HTTP.RequestHeaders (IO LBS.ByteString)
requestGET = request{method = Method.method GET}

requestPOST :: Tree.Request POST [Text] HTTP.Query HTTP.RequestHeaders (IO LBS.ByteString)
requestPOST = request{method = Method.method POST}

requestPUT :: Tree.Request PUT [Text] HTTP.Query HTTP.RequestHeaders (IO LBS.ByteString)
requestPUT = request{method = Method.method PUT}

requestDELETE :: Tree.Request DELETE [Text] HTTP.Query HTTP.RequestHeaders (IO LBS.ByteString)
requestDELETE = request{method = Method.method DELETE}

requestPATCH :: Tree.Request PATCH [Text] HTTP.Query HTTP.RequestHeaders (IO LBS.ByteString)
requestPATCH = request{method = Method.method PATCH}

requestHEAD :: Tree.Request HEAD [Text] HTTP.Query HTTP.RequestHeaders (IO LBS.ByteString)
requestHEAD = request{method = Method.method HEAD}

requestOPTIONS :: Tree.Request OPTIONS [Text] HTTP.Query HTTP.RequestHeaders (IO LBS.ByteString)
requestOPTIONS = request{method = Method.method OPTIONS}

requestCONNECT :: Tree.Request CONNECT [Text] HTTP.Query HTTP.RequestHeaders (IO LBS.ByteString)
requestCONNECT = request{method = Method.method CONNECT}

requestTRACE :: Tree.Request TRACE [Text] HTTP.Query HTTP.RequestHeaders (IO LBS.ByteString)
requestTRACE = request{method = Method.method TRACE}

method ::
    KnownMethod m ->
    Tree.Request HTTP.Method path query headers body ->
    Tree.Request (KnownMethod m) path query headers body
method km r = r{method = Method.method km}

path ::
    SymTree Path path ->
    Tree.Request method [Text] query headers body ->
    Tree.Request method path query headers body
path c r = r{path = c}

query ::
    SymTree Query query ->
    Tree.Request method path HTTP.Query headers body ->
    Tree.Request method path query headers body
query c r = r{query = c}

headers ::
    SymTree Headers headers ->
    Tree.Request method path query HTTP.RequestHeaders body ->
    Tree.Request method path query headers body
headers c r = r{headers = c}

body ::
    Body body ->
    Tree.Request method path query headers (IO LBS.ByteString) ->
    Tree.Request method path query headers body
body c r = r{body = c}

parser ::
    Tree.Request method path query headers body ->
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
    Tree.Request method path query headers body ->
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
    Tree.Request method path query headers body ->
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
                , Wai.requestHeaders = Headers.coalesceCookies (Headers.printer req.headers rv.headers)
                }
    pure (Wai.setRequestBodyChunks streamBody baseReq)
