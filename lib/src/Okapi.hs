{-# LANGUAGE DuplicateRecordFields #-}

module Okapi
    (
      Canopy (..)
    , Signature
    , fn
    , serve
    , tryServe
    , Server (..)
    , Client (..)

    , app
    , client

    , openApi
    , endpointToOpenApi

    , fetch
    , ClientError (..)
    , ClientSettings (..)

    , KnownMethod (..)
    , GET
    , POST
    , PUT
    , DELETE

    , KnownStatus (..)
    , S200
    , S201
    , S204
    , S404
    , S500

    , ArrayStyle (..)

    , Only (..)
    , only

    , Cases
    , Responses
    , getResponses
    , cases

    , parseRequest
    , parseRequestResult
    , printRequest
    , linkTo
    , parseResponse
    , parseResponseResult
    , parseResponses
    , printResponse
    , printResponses

    , Leaf (..)
    , Info (..)
    , HasLeaf (..)
    , (=.)
    , int, int16, int32, int64, integer
    , bool, float, double, scientific, text
    , day, localTime, utcTime, timeOfDay, uuid

    , IsoJson

    , request, methodGET, methodPOST, methodPUT, methodDELETE
    , method, path, query, headers, body
    , pathOf, queryOf, headersOf

    , response, status200, status201, status204, status404, status500

    , segment, segment_, segments
    , param, param', param_, flag, flag', list, list'

    , field, field', field_, contentType, cookie, cookie'
    , fieldStructured, fieldBareItem, fieldItem, fieldList, fieldDictionary

    , attribute, attribute', secure, httpOnly
    ) where

import Okapi.Forest.Canopy (Canopy (..), Signature)
import Okapi.Forest.Server (Server (..), fn, serve, tryServe, app)
import Okapi.Forest.Client (Client (..), ClientError (..), ClientSettings (..), fetch, client)
import Okapi.Artifact.OpenApi (endpointToOpenApi, openApi)
import Okapi.Leaf
    ( Leaf (..), Info (..), HasLeaf (..)
    , int, int16, int32, int64, integer
    , bool, float, double, scientific, text
    , day, localTime, utcTime, timeOfDay, uuid
    )
import Okapi.Tree ((=.))
import Okapi.HTTP.Request
    ( request, methodGET, methodPOST, methodPUT, methodDELETE
    , method, path, query, headers, body
    , pathOf, queryOf, headersOf
    , parseRequest, parseRequestResult, printRequest, linkTo
    )
import Okapi.HTTP.Request.Body (IsoJson)
import Okapi.HTTP.Request.Method (DELETE, GET, KnownMethod (..), POST, PUT)
import Okapi.HTTP.Request.Path (segment, segment_, segments)
import Okapi.HTTP.Request.Query (ArrayStyle (..), param, param', param_, flag, flag', list, list')
import Okapi.HTTP.Request.Headers
    ( field, field', field_, contentType, cookie, cookie'
    , fieldStructured, fieldBareItem, fieldItem, fieldList, fieldDictionary
    )
import Okapi.HTTP.Response
    ( response, status200, status201, status204, status404, status500
    , parseResponse, parseResponseResult, printResponse
    )
import Okapi.HTTP.Response.Status (KnownStatus (..), S200, S201, S204, S404, S500)
import Okapi.HTTP.Responses (Only (..), only, Cases, Responses, getResponses, cases, parseResponses, printResponses)
import Okapi.HTTP.Headers.Attributes (attribute, attribute', secure, httpOnly)
