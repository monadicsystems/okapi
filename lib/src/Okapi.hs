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
    , list
    , list'
    , ArrayStyle (..)
    , deepObject
      -- ** Headers DSL
    , HasHeaders (..)
    , header
    , header'
    , header_
    , cookie
    , cookie'
    , setCookie
    , setCookie'
    , Separator (..)
    , structHeader
    , structHeaderL
    , structHeaderSep
    , ForRequest
    , ForResponse
      -- ** Generic DSL derivation
    , LitF (..)
    , ConstF (..)
    , CookieF (..)
    , SetCookieF (..)
    , pathOf
    , queryOf
    , headersOf
      -- ** Body DSL
    , HasBody (..)
    , json
    , form
    , html
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
    , Cases (..)
    , Responses
    , getResponses
      -- * Codec utilities
    , IsoCodec (isoCodec)
    , Value (..)
    , (=.)
      -- * Testing utilities
    , parseRequest
    , parseRequestResult
    , printRequest
    , linkTo
    , parseResponse
    , parseResponseResult
    , parseResponses
    , printResponse
      -- * Data typeclasses (implement for custom path/query/header types)
    , IsoPathData
    , IsoQueryData
    , IsoHeaderData
    , IsoCookieData
    ) where

import Okapi.Protocol.Shared.Body (HasBody (..), IsoJson, json, form, html, noContent)
import Okapi.Codec (IsoCodec (..), ParseError (..), Result (..), Value (..), (=.))
import Okapi.Data (IsoHeaderData, IsoCookieData, IsoPathData, IsoQueryData)
import Okapi.Protocol.Shared.Headers
    ( ForRequest, ForResponse
    , HasHeaders (..)
    , cookie, cookie', header, header', header_, setCookie, setCookie'
    , ConstF (..), CookieF (..), SetCookieF (..)
    , Separator (..), structHeader, structHeaderL, structHeaderSep
    )
import Okapi.Artifact.OpenApi (endpointToOpenApi, openApi)
import Okapi.Protocol.Request (Request (..), mDelete, mGet, mPost, path, query, request, seg, seg_, pathOf, queryOf, headersOf)
import Okapi.Protocol.Request.Path (LitF (..))
import Okapi.Protocol.Request.Method (DELETE, GET, KnownMethod (..), POST, PUT)
import Okapi.Protocol.Request.Path (segs)
import Okapi.Protocol.Request.Query (flag, flag', param, param', list, list', ArrayStyle (..), deepObject)
import Okapi.Protocol.Response (Response (..), response, s200, s201, s204, s404, s500)
import Okapi.Protocol.Response (Cases (..), Responses, getResponses)
import Okapi.Protocol.Response.Status (KnownStatus (..), S200, S201, S204, S404, S500)
import Okapi.Contract (Signature, Contract (..), parseRequest, parseRequestResult, printRequest, linkTo, parseResponse, parseResponseResult, parseResponses, printResponse)
import Okapi.Application.Server (Server (..), fn, serve, app)
import Okapi.Application.Client (Client (..), ClientError (..), ClientSettings (..), fetch, client)
