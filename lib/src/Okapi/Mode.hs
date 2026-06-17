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

import Control.Applicative ((<|>))
import Data.ByteString qualified as BS
import Data.ByteString.Builder qualified as Builder
import Data.ByteString.Lazy qualified as LBS
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Kind (Type)
import GHC.Generics (Generic (..), Rep)
import Network.HTTP.Types qualified as HTTP
import Network.Wai qualified as Wai
import Network.Wai.Internal qualified as WaiI
import Okapi.Body qualified as Body
import Okapi.Codec (IsoCodec (..), ParseError (..), Result (..), Value (..))
import Okapi.Headers qualified as Headers
import Okapi.Request (Request)
import Okapi.Request qualified as OkReq
import Okapi.Request.Method qualified as Method
import Okapi.Request.Path qualified as Path
import Okapi.Request.Query qualified as Query
import Okapi.Response (Response)
import Okapi.Response qualified as OkRes
import Okapi.Response.Status qualified as Status
import Okapi.Responses
    ( GResponseOut
    , ResponseEnum (..)
    , Responses (..)
    , buildR
    , runGResponseFrom
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
        Responses r (GResponseOut (Rep (r ParseError))) (GResponseOut (Rep (r Value))) ->
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
    let status  = Wai.responseStatus  waiRes
        hdrs    = Wai.responseHeaders waiRes
        bodyLbs = extractWaiResBody   waiRes
        (sr, _) = Status.parse  res.status.isoCodec  status
        (hr, _) = Headers.parse res.headers.isoCodec hdrs
        (br, _) = Body.parse    res.body.isoCodec    bodyLbs
    pure $ case (sr, hr, br) of
        (Right s, Right h, Right b) -> Right (OkRes.response s h b)
        _ -> Left OkRes.Response
            { status  = ParseError (either Just (const Nothing) sr)
            , headers = ParseError (either Just (const Nothing) hr)
            , body    = ParseError (either Just (const Nothing) br)
            }

-- | Parse a single 'Response' codec; always returns per-field 'Result'.
parseResponseResult ::
    Response IsoCodec s h b ->
    Wai.Response ->
    IO (Response Result s h b)
parseResponseResult res waiRes = do
    let status  = Wai.responseStatus  waiRes
        hdrs    = Wai.responseHeaders waiRes
        bodyLbs = extractWaiResBody   waiRes
        (sr, _) = Status.parse  res.status.isoCodec  status
        (hr, _) = Headers.parse res.headers.isoCodec hdrs
        (br, _) = Body.parse    res.body.isoCodec    bodyLbs
    pure OkRes.Response
        { status  = Result sr
        , headers = Result hr
        , body    = Result br
        }

-- | Project a parsed 'Result' response into its per-field 'ParseError' view.
resultToParseError :: Response Result s h b -> Response ParseError s h b
resultToParseError r = OkRes.Response
    { status  = ParseError (either Just (const Nothing) r.status.result)
    , headers = ParseError (either Just (const Nothing) r.headers.result)
    , body    = ParseError (either Just (const Nothing) r.body.result)
    }

-- | Project a parsed 'Result' response into 'Value' mode; succeeds iff every field parsed.
resultToValue :: Response Result s h b -> Maybe (Response Value s h b)
resultToValue r = case (r.status.result, r.headers.result, r.body.result) of
    (Right s, Right h, Right b) -> Just (OkRes.response s h b)
    _                           -> Nothing

-- | Walk the response tree once, parsing each constructor's codec into per-field
--   'Result' and wrapping it into both Either-trees with the same 'Left'/'Right'
--   nesting: the 'ParseError' view (one per constructor) and the 'Value' view.
collect :: Responses r aE aV -> Wai.Response -> IO ([aE], Maybe aV)
collect (Only codec) waiRes = do
    rr <- parseResponseResult codec waiRes
    pure ([resultToParseError rr], resultToValue rr)
collect (Choice l r) waiRes = do
    (el, mvl) <- collect l waiRes
    (er, mvr) <- collect r waiRes
    pure (map Left el ++ map Right er, (Left <$> mvl) <|> (Right <$> mvr))

-- | Parse a response sum type using 'ResponseEnum'; returns all per-constructor errors on failure.
parseResponses ::
    forall m p q h b r.
    ResponseEnum r =>
    Contract (Signature m p q h b r) ->
    Wai.Response ->
    IO (Either [r ParseError] (r Value))
parseResponses (_ :-> tree) waiRes = do
    (es, mv) <- collect tree waiRes
    pure $ maybe (Left (map (buildR @ParseError) es)) (Right . buildR @Value) mv

extractWaiResBody :: Wai.Response -> LBS.ByteString
extractWaiResBody (WaiI.ResponseBuilder _ _ b) = Builder.toLazyByteString b
extractWaiResBody _                            = LBS.empty

printResponse ::
    forall m p q h b r.
    ResponseEnum r =>
    Contract (Signature m p q h b r) ->
    r Value ->
    IO Wai.Response
printResponse (_ :-> tree) rv =
    printResponseAlt tree (runGResponseFrom @(Rep (r Value)) (from rv))

printResponseAlt :: Responses r aE aV -> aV -> IO Wai.Response
printResponseAlt (Only res) rv = do
    bodyBytes <- Body.printM res.body.isoCodec rv.body.value
    pure (Wai.responseLBS
        (Status.print  res.status.isoCodec  rv.status.value)
        (Headers.print res.headers.isoCodec rv.headers.value)
        bodyBytes)
printResponseAlt (Choice l _) (Left  a) = printResponseAlt l a
printResponseAlt (Choice _ r) (Right b) = printResponseAlt r b
