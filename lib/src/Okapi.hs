{-# LANGUAGE DuplicateRecordFields #-}

module Okapi
    (
      Contract (..)
    , Signature
    , fn
    , serve
    , Server (..)
    , Client (..)

    , ParseError (..)
    , Result (..)

    , app
    , client
    , openApi

    , fetch
    , ClientError (..)
    , ClientSettings (..)

    , endpointToOpenApi

    , Request (..)
    , Response (..)

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

    , Cases (..)
    , Responses
    , getResponses

    , IsoCodec (isoCodec)
    , Value (..)
    , (=.)
    , IsoJson

    , parseRequest
    , parseRequestResult
    , printRequest
    , linkTo
    , parseResponse
    , parseResponseResult
    , parseResponses
    , printResponse

    , Iso (..)
    , Data (..)
    , int, int16, int32, int64, integer
    , bool, float, double, text
    , day, localTime, utcTime, timeOfDay, uuid

    , request, methodGET, methodPOST, methodDELETE
    , method, path, query, pathOf, queryOf, headersOf

    , response, status200, status201, status204, status404, status500

    , segment, segment_, segments

    , field, field', field_
    , fieldStructured, fieldBareItem, fieldItem, fieldList, fieldDictionary
    , contentType

    , list, list'

    , cookie, cookie', setCookie
    , attribute, attribute', secure, httpOnly
    ) where

import Okapi.Codec (IsoCodec (..), ParseError (..), Result (..), Value (..), (=.))
import Okapi.Artifact.OpenApi (endpointToOpenApi, openApi)
import Okapi.Protocol.Request
    ( Request (..), method, path, query
    , pathOf, queryOf, headersOf, methodGET, methodPOST, methodDELETE, request
    )
import Okapi.Protocol.Body (IsoJson)
import Okapi.Data
    ( Iso (..), Data (..)
    , int, int16, int32, int64, integer
    , bool, float, double, text
    , day, localTime, utcTime, timeOfDay, uuid
    )
import Okapi.Protocol.Request.Method (DELETE, GET, KnownMethod (..), POST, PUT)
import Okapi.Protocol.Request.Query (ArrayStyle (..), list, list')
import Okapi.Protocol.Response
    ( Response (..), Cases (..), Responses, getResponses
    , response, status200, status201, status204, status404, status500
    )
import Okapi.Protocol.Response.Status (KnownStatus (..), S200, S201, S204, S404, S500)
import Okapi.Protocol.Request.Path (segment, segment_, segments)
import Okapi.Protocol.Headers
    ( field, field', field_, fieldStructured, fieldBareItem, fieldItem
    , fieldList, fieldDictionary, contentType, cookie, cookie', setCookie
    )
import Okapi.Protocol.Headers.Attributes (attribute, attribute', secure, httpOnly)
import Okapi.Contract (Signature, Contract (..), parseRequest, parseRequestResult, printRequest, linkTo, parseResponse, parseResponseResult, parseResponses, printResponse)
import Okapi.Application.Server (Server (..), fn, serve, app)
import Okapi.Application.Client (Client (..), ClientError (..), ClientSettings (..), fetch, client)
