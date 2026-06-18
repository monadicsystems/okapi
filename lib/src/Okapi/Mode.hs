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
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Okapi.Mode (
    Signature,
    Contract (..),
    ClientError (..),
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
    parseResponses,
    printResponse,
    extractWaiResBody,
) where

import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.Functor.Identity (Identity (..))
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Kind (Type)
import Data.List.NonEmpty qualified as NE
import Data.Maybe (mapMaybe)
import Network.HTTP.Types qualified as HTTP
import Network.Wai qualified as Wai
import Okapi.Body qualified as Body
import Okapi.Codec (IsoCodec (..), ParseError (..), Result (..), Value (..))
import Okapi.Headers qualified as Headers
import Okapi.Request (Request)
import Okapi.Request qualified as OkReq
import Okapi.Request.Method qualified as Method
import Okapi.Request.Path qualified as Path
import Okapi.Request.Query qualified as Query
import Okapi.Response (Response)
import Okapi.Responses
    ( ResponseEnum (..)
    , Responses (Responses)
    , extractWaiResBody
    , parseResponseResult
    , printOne
    , resultToParseError
    , resultToValue
    , traverseResponse
    , zipResponse
    )

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
        Responses r ->
        Contract (Signature m p q h b r)

data ClientError = ClientError deriving (Eq, Show)

data Client sig where
    Cb ::
        (Request Value m p q h b -> IO (Either ClientError (r Value))) ->
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
    ResponseEnum r =>
    (n ~> IO) ->
    Contract (Signature m p q h b r) ->
    Server n (Signature m p q h b r) ->
    Wai.Application
serve runner endpoint (Fn handler) waiReq respond = do
    parsed <- parseRequest endpoint waiReq
    case parsed of
        Left _    -> respond (Wai.responseLBS HTTP.status400 [] mempty)
        Right req -> do
            resVal <- runner (handler (req, waiReq))
            waiRes <- printResponse endpoint resVal
            respond waiRes

-- | Attempt to handle a request; passes through to the next middleware on contract mismatch.
tryServe ::
    ResponseEnum r =>
    (n ~> IO) ->
    Contract (Signature m p q h b r) ->
    Server n (Signature m p q h b r) ->
    Wai.Middleware
tryServe runner endpoint (Fn handler) next waiReq respond = do
    parsed <- parseRequest endpoint waiReq
    case parsed of
        Left _    -> next waiReq respond
        Right req -> do
            resVal <- runner (handler (req, waiReq))
            waiRes <- printResponse endpoint resVal
            respond waiRes

-- | Parse all request fields; body is read only on route match.
--   @Left@ carries per-field 'ParseError' info; @Right@ carries parsed values.
parseRequest ::
    Contract (Signature m p q h b r) ->
    Wai.Request ->
    IO (Either (Request ParseError m p q h b) (Request Value m p q h b))
parseRequest (req :-> _) waiReq = do
    let (mr, _) = Method.parse   req.method.isoCodec  (Wai.requestMethod  waiReq)
        pr       = Path.parseExact req.path.isoCodec   (Wai.pathInfo       waiReq)
        (qr, _) = Query.parse    req.query.isoCodec   (Wai.queryString    waiReq)
        (hr, _) = Headers.parse  req.headers.isoCodec (Wai.requestHeaders waiReq)
    case (mr, pr, qr, hr) of
        (Right m, Right p, Right q, Right h) -> do
            bodyRaw <- Wai.strictRequestBody waiReq
            let (brE, _) = Body.parse req.body.isoCodec bodyRaw
            br <- traverse id brE
            pure $ case br of
                Right b -> Right $ OkReq.request m p q h (pure b)
                Left  e -> Left OkReq.Request
                    { method  = ParseError Nothing
                    , path    = ParseError Nothing
                    , query   = ParseError Nothing
                    , headers = ParseError Nothing
                    , body    = ParseError (Just e)
                    }
        _ -> pure $ Left OkReq.Request
            { method  = ParseError (either Just         (const Nothing) mr)
            , path    = ParseError (either (Just . fst) (const Nothing) pr)
            , query   = ParseError (either Just         (const Nothing) qr)
            , headers = ParseError (either Just         (const Nothing) hr)
            , body    = ParseError Nothing
            }

-- | Parse all request fields; always returns per-field 'Result'.
parseRequestResult ::
    Contract (Signature m p q h b r) ->
    Wai.Request ->
    IO (Request Result m p q h b)
parseRequestResult (req :-> _) waiReq = do
    bodyRaw <- Wai.strictRequestBody waiReq
    let (mr, _)  = Method.parse   req.method.isoCodec  (Wai.requestMethod  waiReq)
        pr        = Path.parseExact req.path.isoCodec   (Wai.pathInfo       waiReq)
        (qr, _)  = Query.parse    req.query.isoCodec   (Wai.queryString    waiReq)
        (hr, _)  = Headers.parse  req.headers.isoCodec (Wai.requestHeaders waiReq)
        (brE, _) = Body.parse     req.body.isoCodec    bodyRaw
    pure OkReq.Request
        { method  = Result mr
        , path    = Result (either (Left . fst) Right pr)
        , query   = Result qr
        , headers = Result hr
        , body    = Result brE
        }

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

-- | Parse a single 'Response' codec against a 'Wai.Response', symmetric to 'parseRequest'.
--   @Left@ carries per-field 'ParseError'; @Right@ carries parsed values.
parseResponse ::
    Response IsoCodec s h b ->
    Wai.Response ->
    IO (Either (Response ParseError s h b) (Response Value s h b))
parseResponse res waiRes = do
    rr <- parseResponseResult res waiRes
    pure $ maybe (Left (resultToParseError rr)) Right (resultToValue rr)

-- | Parse a response sum type using 'ResponseEnum'. Tries every constructor's
--   codec; returns the first that fully parses, or all per-constructor errors.
parseResponses ::
    forall m p q h b r.
    ResponseEnum r =>
    Contract (Signature m p q h b r) ->
    Wai.Response ->
    IO (Either [r ParseError] (r Value))
parseResponses (_ :-> Responses cs) waiRes = do
    rs <- traverse parseBranch (NE.toList cs)
    pure $ case mapMaybe toValue rs of
        (v : _) -> Right v
        []      -> Left (map toErrors rs)
  where
    parseBranch :: r IsoCodec -> IO (r Result)
    parseBranch = traverseResponse @IsoCodec @Result (\codec -> parseResponseResult codec waiRes)
    toValue :: r Result -> Maybe (r Value)
    toValue = traverseResponse @Result @Value resultToValue
    toErrors :: r Result -> r ParseError
    toErrors = runIdentity . traverseResponse @Result @ParseError (Identity . resultToParseError)

printResponse ::
    forall m p q h b r.
    ResponseEnum r =>
    Contract (Signature m p q h b r) ->
    r Value ->
    IO Wai.Response
printResponse (_ :-> Responses cs) rv =
    case [io | c <- NE.toList cs, Just io <- [zipResponse @IsoCodec @Value printOne c rv]] of
        (io : _) -> io
        []       -> error "printResponse: no matching response constructor"  -- unreachable: cs covers all constructors
