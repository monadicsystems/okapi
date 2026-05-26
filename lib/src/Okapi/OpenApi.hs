{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Okapi.OpenApi (endpointToOpenApi) where

import Control.Lens ((&), (.~), (?~))
import Data.ByteString.Char8 qualified as BS8
import Data.CaseInsensitive qualified as CI
import Data.HashMap.Strict.InsOrd qualified as IHM
import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import Data.OpenApi (OpenApi, Operation, Param, ParamLocation (..), PathItem (..), Referenced (..), Response, Responses (..))
import Data.OpenApi qualified as OA
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Data.Text qualified as T
import Network.HTTP.Types qualified as HTTP
import Okapi.Codec (Codec (..), IsoCodec (..))
import Okapi.Mode (Endpoint (..), Signature)
import Okapi.Req (body_, headers_, method_, path_, query_)
import Okapi.Req.Body qualified as ReqBody
import Okapi.Req.Headers (Headers)
import Okapi.Req.Headers qualified as ReqH
import Okapi.Req.Method qualified as Method
import Okapi.Req.Path (Path)
import Okapi.Req.Path qualified as Path
import Okapi.Req.Query (Query)
import Okapi.Req.Query qualified as Query
import Okapi.Res (Res, status_)
import Okapi.Res qualified as ORes
import Okapi.Res.Body qualified as ResBody
import Okapi.Res.Headers qualified as ResH
import Okapi.Res.Status qualified as Status
import Okapi.ResAlt (ResAlt (..))

-- ---------------------------------------------------------------------------
-- Path

data PathPiece = PLit Text | PParam Int

walkPath :: Codec Path i o -> ([PathPiece], Int) -> ([PathPiece], Int)
walkPath (Embed (Path.Lit t)) (ps, n) = (ps ++ [PLit t], n)
walkPath (Embed Path.Param)   (ps, n) = (ps ++ [PParam n], n + 1)
walkPath (Embed Path.Blob)    (ps, n) = (ps ++ [PParam n], n + 1)
walkPath (Embed Path.Raw)     s       = s
walkPath (FMap _ c)           s       = walkPath c s
walkPath (LMap _ c)           s       = walkPath c s
walkPath (Apply cf cx)        s       = walkPath cx (walkPath cf s)
walkPath (Pure _)             s       = s

pathTemplate :: [PathPiece] -> FilePath
pathTemplate pieces = "/" <> intercalate "/" (map piece pieces)
  where
    piece (PLit t)   = T.unpack t
    piece (PParam n) = "{param" <> show n <> "}"

pathOAParams :: [PathPiece] -> [Param]
pathOAParams pieces =
    [ mkParam ("param" <> T.pack (show n)) ParamPath True
    | PParam n <- pieces
    ]

-- ---------------------------------------------------------------------------
-- Query

extractQueryParams :: Codec Query i o -> [Param]
extractQueryParams (Embed (Query.Param key))  = [mkParam key ParamQuery True]
extractQueryParams (Embed (Query.Flag key))   = [mkParam key ParamQuery False]
extractQueryParams (Embed (Query.Optional c)) = map makeOptional (extractQueryParams c)
extractQueryParams (Embed Query.Raw)          = []
extractQueryParams (FMap _ c)                 = extractQueryParams c
extractQueryParams (LMap _ c)                 = extractQueryParams c
extractQueryParams (Apply cf cx)              = extractQueryParams cf ++ extractQueryParams cx
extractQueryParams (Pure _)                   = []

-- ---------------------------------------------------------------------------
-- Request headers

extractHeaderParams :: Codec Headers i o -> [Param]
extractHeaderParams (Embed (ReqH.Param key))  = [mkParam (hdrName key) ParamHeader True]
extractHeaderParams (Embed (ReqH.Optional c)) = map makeOptional (extractHeaderParams c)
extractHeaderParams (Embed ReqH.Raw)          = []
extractHeaderParams (FMap _ c)                = extractHeaderParams c
extractHeaderParams (LMap _ c)                = extractHeaderParams c
extractHeaderParams (Apply cf cx)             = extractHeaderParams cf ++ extractHeaderParams cx
extractHeaderParams (Pure _)                  = []

-- ---------------------------------------------------------------------------
-- Response headers

extractResHeaders :: Codec ResH.Headers i o -> [(Text, Bool)]
extractResHeaders (Embed (ResH.Param key))  = [(hdrName key, True)]
extractResHeaders (Embed (ResH.Optional c)) = map (\(n, _) -> (n, False)) (extractResHeaders c)
extractResHeaders (Embed ResH.Raw)          = []
extractResHeaders (FMap _ c)                = extractResHeaders c
extractResHeaders (LMap _ c)                = extractResHeaders c
extractResHeaders (Apply cf cx)             = extractResHeaders cf ++ extractResHeaders cx
extractResHeaders (Pure _)                  = []

-- ---------------------------------------------------------------------------
-- Body detection

isJsonReqBody :: Codec ReqBody.Body i o -> Bool
isJsonReqBody (Embed ReqBody.Json) = True
isJsonReqBody (Embed _)            = False
isJsonReqBody (FMap _ c)           = isJsonReqBody c
isJsonReqBody (LMap _ c)           = isJsonReqBody c
isJsonReqBody (Apply cf cx)        = isJsonReqBody cf || isJsonReqBody cx
isJsonReqBody (Pure _)             = False

isJsonResBody :: Codec ResBody.Body i o -> Bool
isJsonResBody (Embed ResBody.Json) = True
isJsonResBody (Embed _)            = False
isJsonResBody (FMap _ c)           = isJsonResBody c
isJsonResBody (LMap _ c)           = isJsonResBody c
isJsonResBody (Apply cf cx)        = isJsonResBody cf || isJsonResBody cx
isJsonResBody (Pure _)             = False

-- ---------------------------------------------------------------------------
-- ResAlt walker

data ResInfo = ResInfo
    { resStatus     :: HTTP.Status
    , resBodySchema :: Maybe OA.Schema
    , resHdrNames   :: [(Text, Bool)]
    }

extractResInfos :: ResAlt a -> [ResInfo]
extractResInfos (OneResAlt (res :: Res IsoCodec s h b)) =
    [ ResInfo
        { resStatus     = fromMaybe HTTP.status200 (Status.extractStatus (isoCodec (status_ res)))
        , resBodySchema = if isJsonResBody (isoCodec (ORes.body_ res))
                          then Just (OA.toSchema (Proxy @b))
                          else Nothing
        , resHdrNames   = extractResHeaders (isoCodec (ORes.headers_ res))
        }
    ]
extractResInfos (ChoiceResAlt l r) = extractResInfos l ++ extractResInfos r

walkResAltCodec :: Codec ResAlt i o -> [ResInfo]
walkResAltCodec (Embed ra)    = extractResInfos ra
walkResAltCodec (FMap _ c)    = walkResAltCodec c
walkResAltCodec (LMap _ c)    = walkResAltCodec c
walkResAltCodec (Apply cf cx) = walkResAltCodec cf ++ walkResAltCodec cx
walkResAltCodec (Pure _)      = []

-- ---------------------------------------------------------------------------
-- Helpers

hdrName :: HTTP.HeaderName -> Text
hdrName = T.pack . BS8.unpack . CI.original

mkParam :: Text -> ParamLocation -> Bool -> Param
mkParam n loc req_ = mempty
    & OA.name     .~ n
    & OA.in_      .~ loc
    & OA.required ?~ req_

makeOptional :: Param -> Param
makeOptional p = p & OA.required ?~ False

mkResResponse :: ResInfo -> Response
mkResResponse ri =
    mempty
    & OA.description .~ T.pack (show (HTTP.statusCode (resStatus ri)))
    & applyResBodySchema (resBodySchema ri)
    & applyResHeaders (resHdrNames ri)

applyResBodySchema :: Maybe OA.Schema -> Response -> Response
applyResBodySchema Nothing   r = r
applyResBodySchema (Just sc) r = r
    & OA.content .~ IHM.singleton "application/json"
        (mempty & OA.schema ?~ Inline sc)

applyResHeaders :: [(Text, Bool)] -> Response -> Response
applyResHeaders [] r = r
applyResHeaders hs r = r
    & OA.headers .~ IHM.fromList
        [ (name, Inline (mempty & OA.required ?~ req_))
        | (name, req_) <- hs
        ]

applyReqBodySchema :: Maybe OA.Schema -> Operation -> Operation
applyReqBodySchema Nothing   op = op
applyReqBodySchema (Just sc) op = op
    & OA.requestBody ?~ Inline
        ( mempty & OA.content .~ IHM.singleton "application/json"
            (mempty & OA.schema ?~ Inline sc)
        )

setMethod :: HTTP.StdMethod -> Operation -> PathItem -> PathItem
setMethod HTTP.GET    op pi_ = pi_ { _pathItemGet    = Just op }
setMethod HTTP.POST   op pi_ = pi_ { _pathItemPost   = Just op }
setMethod HTTP.PUT    op pi_ = pi_ { _pathItemPut    = Just op }
setMethod HTTP.DELETE op pi_ = pi_ { _pathItemDelete = Just op }
setMethod _           op pi_ = pi_ { _pathItemGet    = Just op }

-- ---------------------------------------------------------------------------
-- Main

endpointToOpenApi
    :: forall m p q h b r
    .  OA.ToSchema b
    => Endpoint (Signature m p q h b r)
    -> OpenApi
endpointToOpenApi (req :-> IsoCodec resAlt) =
    let
        stdMeth     = fromMaybe HTTP.GET (Method.extractMethod (isoCodec (method_ req)))
        (pieces, _) = walkPath (isoCodec (path_ req)) ([], 0)
        qParams     = extractQueryParams (isoCodec (query_ req))
        hParams     = extractHeaderParams (isoCodec (headers_ req))
        resInfos    = walkResAltCodec resAlt
        reqBody     = if stdMeth `notElem` [HTTP.GET, HTTP.HEAD]
                         && isJsonReqBody (isoCodec (body_ req))
                      then Just (OA.toSchema (Proxy @b))
                      else Nothing
        op = mempty
            & OA.parameters .~ map Inline (pathOAParams pieces ++ qParams ++ hParams)
            & OA.responses  .~ Responses Nothing
                (IHM.fromList
                    [ (HTTP.statusCode (resStatus ri), Inline (mkResResponse ri))
                    | ri <- resInfos
                    ])
            & applyReqBodySchema reqBody
    in mempty
        & OA.info . OA.title   .~ "API"
        & OA.info . OA.version .~ "0.1.0"
        & OA.paths .~ IHM.singleton (pathTemplate pieces) (setMethod stdMeth op mempty)
