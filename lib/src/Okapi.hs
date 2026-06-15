{-# LANGUAGE DuplicateRecordFields #-}

{-| Okapi — a type-safe, bidirectional HTTP framework for Haskell.

Define endpoints as 'Contract' values (request codec @:->@ response codec), then derive
WAI applications, HTTP clients, and OpenAPI documents from the same source of truth.
-}
module Okapi
    ( -- * Core
      Contract (..)
    , Signature
    , fn
    , serve
    , Server (..)
    , Client (..)
      -- * HKD modes
    , ParseError (..)
    , Result (..)
      -- * App + client derivation
    , app
    , client
    , openApi
      -- * HTTP client
    , fetch
    , ClientError (..)
    , ClientSettings (..)
      -- * OpenAPI
    , endpointToOpenApi
      -- * Requests
    , Request (..)
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
    , IsoJson
      -- * Responses
    , Response (..)
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
    , ResponseEnum (..)
      -- * Codec utilities
    , IsoCodec (..)
    , Value (..)
    , (=.)
      -- * Testing utilities
    , parseRequest
    , parseRequestResult
    , printRequest
    , parseResponse
    , parseResponseResult
    , printResponse
      -- * Data typeclasses (implement for custom path/query/header types)
    , IsoPathData
    , IsoQueryData
    , IsoHeaderData
    , IsoCookieData
    ) where

import Okapi.Body (HasBody (..), IsoJson, json, noContent)
import Okapi.Client (ClientError (..), ClientSettings (..), fetch)
import Okapi.Codec (IsoCodec (..), ParseError (..), Result (..), Value (..), (=.))
import Okapi.Data (IsoHeaderData, IsoCookieData, IsoPathData, IsoQueryData)
import Okapi.Group (app, client, openApi)
import Okapi.Headers
    ( ForRequest, ForResponse
    , HasHeaders (..)
    , cookie, cookie', header, header', header_, setCookie, setCookie'
    )
import Okapi.Mode
    ( Client (..), Contract (..), Server (..), Signature
    , fn, serve
    , parseRequest, parseRequestResult, printRequest
    , parseResponse, parseResponseResult, printResponse
    )
import Okapi.OpenApi (endpointToOpenApi)
import Okapi.Request (Request (..), mDelete, mGet, mPost, path, query, request, seg, seg_)
import Okapi.Request.Method (DELETE, GET, KnownMethod (..), POST, PUT)
import Okapi.Request.Path (segs)
import Okapi.Request.Query (flag, flag', param, param')
import Okapi.Response (Response (..), response, s200, s201, s204, s404, s500)
import Okapi.Responses (ResponseEnum (..))
import Okapi.Response.Status (KnownStatus (..), S200, S201, S204, S404, S500)
