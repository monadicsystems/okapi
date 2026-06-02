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
    ClientError,
    Endpoint (..),
    Client (..),
    Server (..),
    fn,
    type (~>),
    serve,
    ParseError (..),
    parseRequest,
    printRequest,
    parseResponse,
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
import Okapi.Codec (Codec (..), IsoCodec (..), Value (..))
import Okapi.Req (Req)
import Okapi.Req qualified as OkReq
import Okapi.Req.Body qualified as ReqBody
import Okapi.Req.Headers qualified as ReqHeaders
import Okapi.Req.Method qualified as Method
import Okapi.Req.Path qualified as Path
import Okapi.Req.Query qualified as Query
import Okapi.Res qualified as OkRes
import Okapi.Res.Body qualified as ResBody
import Okapi.Res.Headers qualified as ResHeaders
import Okapi.Res.Status qualified as Status
import Okapi.ResAlt (ResAlt (..))

data
    Signature
        (m :: Type)
        (p :: Type)
        (q :: Type)
        (h :: Type)
        (b :: Type)
        (r :: ((Type -> Type) -> Type -> Type) -> Type)

data ClientError

data Endpoint sig where
    (:->) ::
        Req IsoCodec m p q h b ->
        IsoCodec ResAlt (r Value) ->
        Endpoint (Signature m p q h b r)

data Client sig where
    Cb ::
        (Req Value m p q h b -> IO (Either ClientError (r Value))) ->
        Client (Signature m p q h b r)

data Server n sig where
    Fn ::
        ((Req Value m p q h b, Wai.Request) -> n (r Value)) ->
        Server n (Signature m p q h b r)

fn ::
    ((Req Value m p q h b, Wai.Request) -> n (r Value)) ->
    Server n (Signature m p q h b r)
fn = Fn

type (~>) :: (Type -> Type) -> (Type -> Type) -> Type
type f ~> g = forall a. f a -> g a

serve ::
    (n ~> IO) ->
    Endpoint (Signature m p q h b r) ->
    Server n (Signature m p q h b r) ->
    Wai.Application
serve runner endpoint (Fn handler) waiReq respond =
    case parseRequest endpoint waiReq of
        Left _    -> respond (Wai.responseLBS HTTP.status400 [] mempty)
        Right req -> do
            resVal <- runner (handler (req, waiReq))
            waiRes <- printResponse endpoint resVal
            respond waiRes

data ParseError
    = MethodParseError
    | PathParseError
    | QueryParseError
    | HeadersParseError
    | StatusParseError
    | BodyParseError
    deriving (Eq, Show)

parseRequest ::
    Endpoint (Signature m p q h b r) ->
    Wai.Request ->
    Either ParseError (Req Value m p q h b)
parseRequest (req :-> _) waiReq =
    let meth   = Wai.requestMethod  waiReq
        path   = Wai.pathInfo       waiReq
        query  = Wai.queryString    waiReq
        hdrs   = Wai.requestHeaders waiReq
        bodyIO = makeReqBodyIO req.body_.isoCodec waiReq
        (mr, _) = Method.parse     req.method_.isoCodec  meth
        (pr, _) = Path.parse       req.path_.isoCodec    path
        (qr, _) = Query.parse      req.query_.isoCodec   query
        (hr, _) = ReqHeaders.parse req.headers_.isoCodec hdrs
    in case (mr, pr, qr, hr) of
        (Right m, Right p, Right q, Right h) -> Right $ OkReq.value m p q h bodyIO
        (Left _, _, _, _) -> Left MethodParseError
        (_, Left _, _, _) -> Left PathParseError
        (_, _, Left _, _) -> Left QueryParseError
        _                 -> Left HeadersParseError

makeReqBodyIO :: Codec ReqBody.Body (IO b) (IO b) -> Wai.Request -> IO b
makeReqBodyIO (Embed ReqBody.Raw)  waiReq = Wai.strictRequestBody waiReq
makeReqBodyIO (Embed ReqBody.Json) waiReq =
    Wai.strictRequestBody waiReq >>= \bs ->
        case ReqBody.parse (Embed ReqBody.Json) bs of
            (Left _,  _) -> fail "JSON body parse error"
            (Right b, _) -> b
makeReqBodyIO (Pure x) _ = x
makeReqBodyIO c waiReq =
    Wai.strictRequestBody waiReq >>= \bs ->
        case ReqBody.parse c bs of
            (Left _,  _) -> fail "body parse error"
            (Right b, _) -> b

printRequest ::
    Endpoint (Signature m p q h b r) ->
    Req Value m p q h b ->
    IO Wai.Request
printRequest (req :-> _) rv = do
    bodyBytes <- ReqBody.printM req.body_.isoCodec rv.body_.value
    bodyRef   <- newIORef (LBS.toChunks bodyBytes)
    let streamBody = do
            chunks <- readIORef bodyRef
            case chunks of
                []     -> pure BS.empty
                (c:cs) -> writeIORef bodyRef cs >> pure c
    let baseReq = Wai.defaultRequest
            { Wai.requestMethod  = Method.print     req.method_.isoCodec   rv.method_.value
            , Wai.pathInfo       = Path.print       req.path_.isoCodec     rv.path_.value
            , Wai.queryString    = Query.print      req.query_.isoCodec    rv.query_.value
            , Wai.requestHeaders = ReqHeaders.print req.headers_.isoCodec  rv.headers_.value
            }
    pure (Wai.setRequestBodyChunks streamBody baseReq)

parseResponse ::
    Endpoint (Signature m p q h b r) ->
    Wai.Response ->
    Either ParseError (r Value)
parseResponse (_ :-> IsoCodec resCodec) waiRes =
    let status = Wai.responseStatus  waiRes
        hdrs   = Wai.responseHeaders waiRes
        body   = extractWaiResBody   waiRes
    in fst (parseResCodec resCodec (status, hdrs, body))

extractWaiResBody :: Wai.Response -> LBS.ByteString
extractWaiResBody (WaiI.ResponseBuilder _ _ b) = Builder.toLazyByteString b
extractWaiResBody _                            = LBS.empty

printResponse ::
    Endpoint (Signature m p q h b r) ->
    r Value ->
    IO Wai.Response
printResponse (_ :-> IsoCodec resCodec) rv = do
    (status, hdrs, bodyBytes) <- printResCodec resCodec rv
    pure (Wai.responseLBS status hdrs bodyBytes)

type ResState = (HTTP.Status, HTTP.ResponseHeaders, LBS.ByteString)

parseResCodec :: Codec ResAlt i o -> ResState -> (Either ParseError o, ResState)
parseResCodec = go
  where
    go :: forall i' o'. Codec ResAlt i' o' -> ResState -> (Either ParseError o', ResState)
    go (Pure x)      s = (Right x, s)
    go (FMap f c)    s = case go c s of
        (Left e,  s') -> (Left e,  s')
        (Right x, s') -> (Right (f x), s')
    go (LMap _ c)    s = go c s
    go (Apply cf cx) s = case go cf s of
        (Left e, s1)  -> (Left e, s1)
        (Right f, s1) -> case go cx s1 of
            (Left e, s2)  -> (Left e, s2)
            (Right x, s2) -> (Right (f x), s2)
    go (Embed ra)    s = resAltParseAlg ra s

resAltParseAlg :: ResAlt a -> ResState -> (Either ParseError a, ResState)
resAltParseAlg (OneResAlt res) (status, hdrs, body) =
    let (sr, _)     = Status.parse     res.status_.isoCodec   status
        (hr, hdrs') = ResHeaders.parse res.headers_.isoCodec  hdrs
        (br, _)     = ResBody.parse    res.body_.isoCodec     body
    in case (sr, hr, br) of
        (Right s, Right h, Right b) ->
            (Right (OkRes.value s h b), (status, hdrs', LBS.empty))
        (Left _, _, _) -> (Left StatusParseError,  (status, hdrs, body))
        (_, Left _, _) -> (Left HeadersParseError, (status, hdrs, body))
        _              -> (Left BodyParseError,    (status, hdrs, body))
resAltParseAlg (ChoiceResAlt l r) inp =
    case resAltParseAlg l inp of
        (Right a, inp') -> (Right (Left a),  inp')
        (Left _,  _)    -> case resAltParseAlg r inp of
            (Right b, inp') -> (Right (Right b), inp')
            (Left e,  inp') -> (Left e, inp')

printResCodec :: Codec ResAlt i o -> i -> IO ResState
printResCodec = go
  where
    go :: forall i' o'. Codec ResAlt i' o' -> i' -> IO ResState
    go (Pure _)      _ = pure (HTTP.status200, [], LBS.empty)
    go (FMap _ c)    x = go c x
    go (LMap f c)    x = go c (f x)
    go (Apply cf _)  x = go cf x
    go (Embed ra)    x = resAltPrintAlg ra x

resAltPrintAlg :: ResAlt a -> a -> IO ResState
resAltPrintAlg (OneResAlt res) rv = do
    bodyBytes <- ResBody.printM res.body_.isoCodec rv.body_.value
    pure
        ( Status.print     res.status_.isoCodec   rv.status_.value
        , ResHeaders.print res.headers_.isoCodec  rv.headers_.value
        , bodyBytes
        )
resAltPrintAlg (ChoiceResAlt l _) (Left  a) = resAltPrintAlg l a
resAltPrintAlg (ChoiceResAlt _ r) (Right b) = resAltPrintAlg r b
