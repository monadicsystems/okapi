{-# LANGUAGE DuplicateRecordFields #-}

module Okapi
    ( -- * Core
      Contract (..)
    , Signature
    , ParseError
    , fn
    , serve
    , Server (..)
    , Client (..)
      -- * App + client derivation
    , app
    , client
    , openApi
      -- * HTTP client
    , fetch
    , ClientSettings (..)
      -- * OpenAPI
    , endpointToOpenApi
      -- * Requests
    , Request(method_, path_, query_, headers_, body_)
    , request
    , mGet
    , mPost
    , mDelete
      -- ** Method types
    , KnownMethod (..)
    , GET
    , POST
    , PUT
    , DELETE
      -- ** Path DSL
    , path
    , seg
    , seg_
    , segs
      -- ** Query DSL
    , query
    , param
    , param'
    , flag
    , flag'
      -- ** Headers DSL
    , HasHeaders (..)
    , header
    , header'
    , header_
    , cookie
    , cookie'
    , setCookie
    , setCookie'
    , ForRequest
    , ForResponse
      -- ** Body DSL
    , HasBody (..)
    , json
    , noContent
    , NoContent (..)
    , IsoJson
      -- * Responses
    , Response(status_, headers_, body_)
    , response
    , s200
    , s201
    , s204
    , s404
    , s500
    , KnownStatus (..)
    , S200
    , S201
    , S204
    , S404
    , S500
      -- * Response alternatives
    , GenericResAlt (..)
    , Only (..)
    , only
      -- * Codec utilities
    , IsoCodec (..)
    , Value (..)
    , (=.)
      -- * Testing utilities
    , parseRequest
    , printRequest
    , parseResponse
    , printResponse
      -- * Data typeclasses (implement for custom path/query/header types)
    , IsoPathData
    , IsoQueryData
    , IsoHeaderData
    , IsoCookieData
    ) where

import Okapi.Body (HasBody (..), IsoJson, NoContent (..), json, noContent)
import Okapi.Client (ClientSettings (..), fetch)
import Okapi.Codec (IsoCodec (..), Value (..), (=.))
import Okapi.Data (IsoHeaderData, IsoCookieData, IsoPathData, IsoQueryData)
import Okapi.Group (app, client, openApi)
import Okapi.Headers
    ( ForRequest, ForResponse
    , HasHeaders (..)
    , cookie, cookie', header, header', header_, setCookie, setCookie'
    )
import Okapi.Mode
    ( Client (..), Contract (..), ParseError, Server (..), Signature
    , fn, serve
    , parseRequest, printRequest, parseResponse, printResponse
    )
import Okapi.OpenApi (endpointToOpenApi)
import Okapi.Request (Request, body_, headers_, method_, mDelete, mGet, mPost, path, path_, query, query_, request, seg, seg_)
import Okapi.Request.Method (DELETE, GET, KnownMethod (..), POST, PUT)
import Okapi.Request.Path (segs)
import Okapi.Request.Query (flag, flag', param, param')
import Okapi.Response (Response, body_, headers_, response, s200, s201, s204, s404, s500, status_)
import Okapi.Response.Choice (GenericResAlt (..), Only (..), only)
import Okapi.Response.Status (KnownStatus (..), S200, S201, S204, S404, S500)
