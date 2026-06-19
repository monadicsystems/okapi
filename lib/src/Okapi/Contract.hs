{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
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

module Okapi.Contract (
    Signature,
    Contract (..),
    parseRequest,
    parseRequestResult,
    printRequest,
    linkTo,
    parseResponse,
    parseResponseResult,
    parseResponses,
    printResponse,
) where

import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.Functor.Identity (Identity (..))
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Kind (Type)
import Data.List.NonEmpty qualified as NE
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8)
import Network.HTTP.Types qualified as HTTP
import Network.Wai qualified as Wai
import Okapi.Protocol.Shared.Body qualified as Body
import Okapi.Codec (IsoCodec (..), ParseError (..), Result (..), Value (..))
import Okapi.Protocol.Shared.Headers qualified as Headers
import Okapi.Protocol.Request (Request)
import Okapi.Protocol.Request qualified as OkReq
import Okapi.Protocol.Request.Method qualified as Method
import Okapi.Protocol.Request.Path qualified as Path
import Okapi.Protocol.Request.Query qualified as Query
import Okapi.Protocol.Response
    ( Response
    , Cases (..)
    , Responses (Responses)
    , parseResponseResult
    , printOne
    , resultToParseError
    , resultToValue
    , traverseResponses
    , zipResponses
    )


-- | Type-level tag encoding the method, path, query, header, body, and response shape of one endpoint.
data Signature
    (method    :: Type)
    (path      :: Type)
    (query     :: Type)
    (headers   :: Type)
    (body      :: Type)
    (responses :: ((Type -> Type) -> Type -> Type) -> Type)

-- | An endpoint contract: a request codec paired with a response codec via @:->@.
data Contract sig where
    (:->) ::
        Request IsoCodec method path query headers body ->
        Responses IsoCodec responses ->
        Contract (Signature method path query headers body responses)

-- | Parse all request fields; body is read only on route match.
--   @Left@ carries per-field 'ParseError' info; @Right@ carries parsed values.
parseRequest ::
    Contract (Signature method path query headers body responses) ->
    Wai.Request ->
    IO (Either (Request ParseError method path query headers body) (Request Value method path query headers body))
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
    Contract (Signature method path query headers body responses) ->
    Wai.Request ->
    IO (Request Result method path query headers body)
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
    Contract (Signature method path query headers body responses) ->
    Request Value method path query headers body ->
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

-- | Render the typesafe URL — path and query string — for a contract from a
--   request value. Method, headers, and body are ignored. The result is a
--   relative reference (@\/path?query@); prepend your origin for an absolute URL
--   (e.g. for Stripe @success_url@ / @cancel_url@, or an htmx @hx-get@).
linkTo ::
    Contract (Signature method path query headers body responses) ->
    Request Value method path query headers body ->
    Text
linkTo (req :-> _) rv =
    let segs  = Path.print  req.path.isoCodec  rv.path.value
        query = Query.print req.query.isoCodec rv.query.value
    in "/" <> T.intercalate "/" segs <> decodeUtf8 (HTTP.renderQuery True query)

-- | Parse a single 'Response' codec against a 'Wai.Response', symmetric to 'parseRequest'.
--   @Left@ carries per-field 'ParseError'; @Right@ carries parsed values.
parseResponse ::
    Response IsoCodec status headers body ->
    Wai.Response ->
    IO (Either (Response ParseError status headers body) (Response Value status headers body))
parseResponse res waiRes = do
    rr <- parseResponseResult res waiRes
    pure $ maybe (Left (resultToParseError rr)) Right (resultToValue rr)

-- | Parse a response sum type using 'Cases'. Tries every constructor's
--   codec; returns the first that fully parses, or all per-constructor errors.
parseResponses ::
    forall method path query headers body responses.
    Cases responses =>
    Contract (Signature method path query headers body responses) -> Wai.Response ->
    IO (Either (Responses ParseError responses) (responses Value))
parseResponses (_ :-> Responses cs) waiRes = do
    rs <- traverse parseBranch cs
    pure $ case mapMaybe toValue (NE.toList rs) of
        (v : _) -> Right v
        []      -> Left (Responses (fmap toErrors rs))
  where
    parseBranch :: responses IsoCodec -> IO (responses Result)
    parseBranch = traverseResponses @IsoCodec @Result (\codec -> parseResponseResult codec waiRes)
    toValue :: responses Result -> Maybe (responses Value)
    toValue = traverseResponses @Result @Value resultToValue
    toErrors :: responses Result -> responses ParseError
    toErrors = runIdentity . traverseResponses @Result @ParseError (Identity . resultToParseError)

printResponse ::
    forall method path query headers body responses.
    Cases responses =>
    Contract (Signature method path query headers body responses) ->
    responses Value ->
    IO Wai.Response
printResponse (_ :-> Responses cs) rv =
    case [io | c <- NE.toList cs, Just io <- [zipResponses @IsoCodec @Value printOne c rv]] of
        (io : _) -> io
        []       -> error "printResponse: no matching response constructor"  -- unreachable: cs covers all constructors
