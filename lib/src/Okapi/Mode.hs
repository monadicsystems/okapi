{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Okapi.Mode (
    Signature,
    Contract (..),
    Client (..),
    Server (..),
    fn,
    type (~>),
    serve,
    tryServe,
    parseRequest,
    parseRequestResult,
    printRequest,
    parseResponse,
    parseResponseResult,
    printResponse,
) where

import Data.ByteString qualified as BS
import Data.ByteString.Builder qualified as Builder
import Data.ByteString.Lazy qualified as LBS
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Kind (Type)
import Network.HTTP.Types qualified as HTTP
import Network.Wai qualified as Wai
import Network.Wai.Internal qualified as WaiI
import Okapi.Body (Body, ForRequest)
import Okapi.Body qualified as Body
import Okapi.Codec (Codec (..), IsoCodec (..), ParseError (..), Result (..), Value (..))
import Okapi.Headers qualified as Headers
import Okapi.Request (Request)
import Okapi.Request qualified as OkReq
import Okapi.Request.Method qualified as Method
import Okapi.Request.Path qualified as Path
import Okapi.Request.Query qualified as Query
import Okapi.Response (Response)
import Okapi.Response qualified as OkRes
import Okapi.Response.Status qualified as Status
import Okapi.Responses (Responses (..))

-- | Type-level tag encoding the method, path, query, header, body, and response shape of one endpoint.
data
    Signature
        (m :: Type)
        (p :: Type)
        (q :: Type)
        (h :: Type)
        (b :: Type)
        (r :: ((Type -> Type) -> Type -> Type) -> Type)

-- | An endpoint contract: a request codec paired with a response codec via @:->@.
data Contract sig where
    (:->) ::
        Request IsoCodec m p q h b ->
        IsoCodec Responses (r Value) ->
        Contract (Signature m p q h b r)

data Client sig where
    Cb ::
        (Request Value m p q h b -> IO (Maybe (r Value))) ->
        Client (Signature m p q h b r)

data Server n sig where
    Fn ::
        ((Request Value m p q h b, Wai.Request) -> n (r Value)) ->
        Server n (Signature m p q h b r)

-- | Lift a handler function into a 'Server'.
fn ::
    ((Request Value m p q h b, Wai.Request) -> n (r Value)) ->
    Server n (Signature m p q h b r)
fn = Fn

type (~>) :: (Type -> Type) -> (Type -> Type) -> Type
type f ~> g = forall a. f a -> g a

-- | Build a WAI 'Wai.Application' from a single contract and its handler; returns 400 on parse failure.
serve ::
    (n ~> IO) ->
    Contract (Signature m p q h b r) ->
    Server n (Signature m p q h b r) ->
    Wai.Application
serve runner endpoint (Fn handler) waiReq respond =
    case parseRequest endpoint waiReq of
        Left _    -> respond (Wai.responseLBS HTTP.status400 [] mempty)
        Right req -> do
            resVal <- runner (handler (req, waiReq))
            waiRes <- printResponse endpoint resVal
            respond waiRes

-- | Attempt to handle a request; passes through to the next middleware on contract mismatch.
tryServe ::
    (n ~> IO) ->
    Contract (Signature m p q h b r) ->
    Server n (Signature m p q h b r) ->
    Wai.Middleware
tryServe runner endpoint (Fn handler) next waiReq respond =
    case parseRequest endpoint waiReq of
        Left _    -> next waiReq respond
        Right req -> do
            resVal <- runner (handler (req, waiReq))
            waiRes <- printResponse endpoint resVal
            respond waiRes

-- | Parse all request fields; @Left@ carries per-field 'ParseError' info, @Right@ carries parsed values.
parseRequest ::
    Contract (Signature m p q h b r) ->
    Wai.Request ->
    Either (Request ParseError m p q h b) (Request Value m p q h b)
parseRequest (req :-> _) waiReq =
    let meth   = Wai.requestMethod  waiReq
        path   = Wai.pathInfo       waiReq
        query  = Wai.queryString    waiReq
        hdrs   = Wai.requestHeaders waiReq
        bodyIO = makeReqBodyIO req.body.isoCodec waiReq
        (mr, _) = Method.parse   req.method.isoCodec  meth
        pr       = Path.parseExact req.path.isoCodec    path
        (qr, _) = Query.parse    req.query.isoCodec   query
        (hr, _) = Headers.parse  req.headers.isoCodec hdrs
        errReq = OkReq.Request
            { method  = ParseError (either Just         (const Nothing) mr)
            , path    = ParseError (either (Just . fst) (const Nothing) pr)
            , query   = ParseError (either Just         (const Nothing) qr)
            , headers = ParseError (either Just         (const Nothing) hr)
            , body    = ParseError Nothing
            }
    in case (mr, pr, qr, hr) of
        (Right m, Right p, Right q, Right h) -> Right $ OkReq.request m p q h bodyIO
        _                                    -> Left errReq

-- | Parse all request fields; always returns per-field 'Either' via 'Result'.
parseRequestResult ::
    Contract (Signature m p q h b r) ->
    Wai.Request ->
    Request Result m p q h b
parseRequestResult (req :-> _) waiReq =
    let meth   = Wai.requestMethod  waiReq
        path   = Wai.pathInfo       waiReq
        query  = Wai.queryString    waiReq
        hdrs   = Wai.requestHeaders waiReq
        bodyIO = makeReqBodyIO req.body.isoCodec waiReq
        (mr, _) = Method.parse   req.method.isoCodec  meth
        pr       = Path.parseExact req.path.isoCodec    path
        (qr, _) = Query.parse    req.query.isoCodec   query
        (hr, _) = Headers.parse  req.headers.isoCodec hdrs
    in OkReq.Request
        { method  = Result mr
        , path    = Result (either (Left . fst) Right pr)
        , query   = Result qr
        , headers = Result hr
        , body    = Result (Right bodyIO)
        }

makeReqBodyIO :: Codec (Body ForRequest) (IO b) (IO b) -> Wai.Request -> IO b
makeReqBodyIO (Lift Body.Raw)       waiReq = Wai.strictRequestBody waiReq
makeReqBodyIO (Lift Body.Json)      waiReq =
    Wai.strictRequestBody waiReq >>= \bs ->
        case Body.parse (Lift Body.Json) bs of
            (Left _,  _) -> fail "JSON body parse error"
            (Right b, _) -> b
makeReqBodyIO (Lift Body.NoContent) _ = pure ()
makeReqBodyIO (Pure x) _ = x
makeReqBodyIO c waiReq =
    Wai.strictRequestBody waiReq >>= \bs ->
        case Body.parse c bs of
            (Left _,  _) -> fail "body parse error"
            (Right b, _) -> b

printRequest ::
    Contract (Signature m p q h b r) ->
    Request Value m p q h b ->
    IO Wai.Request
printRequest (req :-> _) rv = do
    bodyBytes <- Body.printM req.body.isoCodec rv.body.value
    bodyRef   <- newIORef (LBS.toChunks bodyBytes)
    let streamBody = do
            chunks <- readIORef bodyRef
            case chunks of
                []     -> pure BS.empty
                (c:cs) -> writeIORef bodyRef cs >> pure c
    let baseReq = Wai.defaultRequest
            { Wai.requestMethod  = Method.print  req.method.isoCodec   rv.method.value
            , Wai.pathInfo       = Path.print    req.path.isoCodec     rv.path.value
            , Wai.queryString    = Query.print   req.query.isoCodec    rv.query.value
            , Wai.requestHeaders = Headers.print req.headers.isoCodec  rv.headers.value
            }
    pure (Wai.setRequestBodyChunks streamBody baseReq)

parseResponse ::
    Contract (Signature m p q h b r) ->
    Wai.Response ->
    Maybe (r Value)
parseResponse (_ :-> IsoCodec resCodec) waiRes =
    let status = Wai.responseStatus  waiRes
        hdrs   = Wai.responseHeaders waiRes
        body   = extractWaiResBody   waiRes
    in fst (parseResponseCodec resCodec (status, hdrs, body))

-- | Parse a single response codec into per-field 'Result' values.
parseResponseResult ::
    Response IsoCodec s h b ->
    Wai.Response ->
    Response Result s h b
parseResponseResult res waiRes =
    let status = Wai.responseStatus  waiRes
        hdrs   = Wai.responseHeaders waiRes
        body   = extractWaiResBody   waiRes
        (sr, _) = Status.parse  res.status.isoCodec  status
        (hr, _) = Headers.parse res.headers.isoCodec hdrs
        (br, _) = Body.parse    res.body.isoCodec    body
    in OkRes.Response
        { status  = Result sr
        , headers = Result hr
        , body    = Result br
        }

extractWaiResBody :: Wai.Response -> LBS.ByteString
extractWaiResBody (WaiI.ResponseBuilder _ _ b) = Builder.toLazyByteString b
extractWaiResBody _                            = LBS.empty

printResponse ::
    Contract (Signature m p q h b r) ->
    r Value ->
    IO Wai.Response
printResponse (_ :-> IsoCodec resCodec) rv = do
    (status, hdrs, bodyBytes) <- printResponseCodec resCodec rv
    pure (Wai.responseLBS status hdrs bodyBytes)

type ResState = (HTTP.Status, [HTTP.Header], LBS.ByteString)

parseResponseCodec :: Codec Responses i o -> ResState -> (Maybe o, ResState)
parseResponseCodec = go
  where
    go :: forall i' o'. Codec Responses i' o' -> ResState -> (Maybe o', ResState)
    go (Pure x)      s = (Just x, s)
    go (FMap f c)    s = case go c s of
        (Nothing, s') -> (Nothing, s')
        (Just x,  s') -> (Just (f x), s')
    go (LMap _ c)    s = go c s
    go (Apply cf cx) s = case go cf s of
        (Nothing, s1) -> (Nothing, s1)
        (Just f,  s1) -> case go cx s1 of
            (Nothing, s2) -> (Nothing, s2)
            (Just x,  s2) -> (Just (f x), s2)
    go (Lift ra)    s = parseResponseAlt ra s

parseResponseAlt :: Responses a -> ResState -> (Maybe a, ResState)
parseResponseAlt (Only res) (status, hdrs, bodyLbs) =
    let (sr, _)     = Status.parse     res.status.isoCodec   status
        (hr, hdrs') = Headers.parse    res.headers.isoCodec  hdrs
        (br, _)     = Body.parse       res.body.isoCodec     bodyLbs
    in case (sr, hr, br) of
        (Right s, Right h, Right b) ->
            (Just (OkRes.response s h b), (status, hdrs', LBS.empty))
        _ -> (Nothing, (status, hdrs, bodyLbs))
parseResponseAlt (Choice l r) inp =
    case parseResponseAlt l inp of
        (Just a, inp')  -> (Just (Left a),  inp')
        (Nothing, _)    -> case parseResponseAlt r inp of
            (Just b,  inp') -> (Just (Right b), inp')
            (Nothing, inp') -> (Nothing, inp')

printResponseCodec :: Codec Responses i o -> i -> IO ResState
printResponseCodec = go
  where
    go :: forall i' o'. Codec Responses i' o' -> i' -> IO ResState
    go (Pure _)      _ = pure (HTTP.status200, [], LBS.empty)
    go (FMap _ c)    x = go c x
    go (LMap f c)    x = go c (f x)
    go (Apply cf _)  x = go cf x
    go (Lift ra)    x = printResponseAlt ra x

printResponseAlt :: Responses a -> a -> IO ResState
printResponseAlt (Only res) rv = do
    bodyBytes <- Body.printM res.body.isoCodec rv.body.value
    pure
        ( Status.print     res.status.isoCodec   rv.status.value
        , Headers.print    res.headers.isoCodec  rv.headers.value
        , bodyBytes
        )
printResponseAlt (Choice l _) (Left  a) = printResponseAlt l a
printResponseAlt (Choice _ r) (Right b) = printResponseAlt r b
