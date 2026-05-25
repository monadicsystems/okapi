{-# LANGUAGE OverloadedStrings #-}

module Okapi.OpenApi (endpointToOpenApi) where

import Control.Lens ((&), (.~), (?~))
import Data.HashMap.Strict.InsOrd qualified as IHM
import Data.List (intercalate)
import Data.Maybe (maybeToList)
import Data.OpenApi (OpenApi, Operation, Param, ParamLocation (..), PathItem (..), Referenced (..), Response, Responses (..))
import Data.OpenApi qualified as OA
import Data.Text (Text)
import Data.Text qualified as T
import Network.HTTP.Types qualified as HTTP
import Okapi.Codec (Codec (..), IsoCodec (..))
import Okapi.Mode (Endpoint (..))
import Okapi.Req (headers_, method_, path_, query_)
import Okapi.Req.Headers (Headers)
import Okapi.Req.Headers qualified as ReqH
import Okapi.Req.Method qualified as Method
import Okapi.Req.Path (Path)
import Okapi.Req.Path qualified as Path
import Okapi.Req.Query (Query)
import Okapi.Req.Query qualified as Query
import Okapi.Res (status_)
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
extractHeaderParams (Embed (ReqH.Param key))  = [mkParam (T.pack (show key)) ParamHeader True]
extractHeaderParams (Embed (ReqH.Optional c)) = map makeOptional (extractHeaderParams c)
extractHeaderParams (Embed ReqH.Raw)          = []
extractHeaderParams (FMap _ c)                = extractHeaderParams c
extractHeaderParams (LMap _ c)                = extractHeaderParams c
extractHeaderParams (Apply cf cx)             = extractHeaderParams cf ++ extractHeaderParams cx
extractHeaderParams (Pure _)                  = []

-- ---------------------------------------------------------------------------
-- ResAlt statuses

extractStatuses :: ResAlt a -> [HTTP.Status]
extractStatuses (OneResAlt res)    = maybeToList (Status.extractStatus (isoCodec (status_ res)))
extractStatuses (ChoiceResAlt l r) = extractStatuses l ++ extractStatuses r

walkResAltCodec :: Codec ResAlt i o -> [HTTP.Status]
walkResAltCodec (Embed ra)    = extractStatuses ra
walkResAltCodec (FMap _ c)    = walkResAltCodec c
walkResAltCodec (LMap _ c)    = walkResAltCodec c
walkResAltCodec (Apply cf cx) = walkResAltCodec cf ++ walkResAltCodec cx
walkResAltCodec (Pure _)      = []

-- ---------------------------------------------------------------------------
-- Helpers

mkParam :: Text -> ParamLocation -> Bool -> Param
mkParam n loc req_ = mempty
    & OA.name      .~ n
    & OA.in_       .~ loc
    & OA.required  ?~ req_

makeOptional :: Param -> Param
makeOptional p = p & OA.required ?~ False

mkResponse :: HTTP.Status -> Response
mkResponse s = mempty & OA.description .~ T.pack (show (HTTP.statusCode s))

setMethod :: HTTP.StdMethod -> Operation -> PathItem -> PathItem
setMethod HTTP.GET    op pi_ = pi_ { _pathItemGet    = Just op }
setMethod HTTP.POST   op pi_ = pi_ { _pathItemPost   = Just op }
setMethod HTTP.PUT    op pi_ = pi_ { _pathItemPut    = Just op }
setMethod HTTP.DELETE op pi_ = pi_ { _pathItemDelete = Just op }
setMethod _           op pi_ = pi_ { _pathItemGet    = Just op }

-- ---------------------------------------------------------------------------
-- Main

endpointToOpenApi :: Endpoint sig -> OpenApi
endpointToOpenApi (req :-> IsoCodec resAlt) =
    let
        stdMeth       = maybe HTTP.GET id (Method.extractMethod (isoCodec (method_ req)))
        (pieces, _)   = walkPath (isoCodec (path_ req)) ([], 0)
        qParams       = extractQueryParams (isoCodec (query_ req))
        hParams       = extractHeaderParams (isoCodec (headers_ req))
        statuses      = walkResAltCodec resAlt
        op = mempty
            & OA.parameters .~ map Inline (pathOAParams pieces ++ qParams ++ hParams)
            & OA.responses  .~ Responses Nothing
                (IHM.fromList
                    [ (HTTP.statusCode s, Inline (mkResponse s))
                    | s <- statuses
                    ])
    in mempty
        & OA.info . OA.title   .~ "API"
        & OA.info . OA.version .~ "0.1.0"
        & OA.paths .~ IHM.singleton (pathTemplate pieces) (setMethod stdMeth op mempty)
