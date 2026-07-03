module Okapi
    (
      Forest (..)
    , Shape
    , fn
    , serve
    , tryServe
    , Server (..)
    , Client (..)

    , Route (..)
    , Handle (..)
    , handle

    , server
    , client

    , openApi
    , endpointToOpenApi

    , type (~>)
    , fetch
    , ClientError (..)
    , ClientSettings (..)

    , URI (..)
    , Link (..)
    , links

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

    , Cases
    , Responses
    , getResponses
    , cases

    , parseRequest
    , parseRequestResult
    , printRequest
    , link
    , parseResponse
    , parseResponseResult
    , parseResponses
    , printResponse
    , printResponses

    , SymTree
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
    , fieldRFC9651, fieldBareItem, fieldItem, fieldList, fieldDictionary

    , attribute, attribute', secure, httpOnly
    ) where

import Okapi.Mode.Forest (Forest (..), Shape)
import Okapi.Mode.Server (Server (..), fn, type (~>), serve, tryServe, server)
import Okapi.Handle (Route (..), Handle (..), handle)
import Okapi.Mode.Client (Client (..), ClientError (..), ClientSettings (..), fetch, client)
import Okapi.Mode.Link (URI (..), Link (..), links)
import Okapi.Artifact.OpenApi (endpointToOpenApi, openApi)
import Okapi.Tree
    ( SymTree
    , Leaf (..), Info (..), HasLeaf (..)
    , int, int16, int32, int64, integer
    , bool, float, double, scientific, text
    , day, localTime, utcTime, timeOfDay, uuid
    , (=.)
    )
import Okapi.HTTP.Request
    ( request, methodGET, methodPOST, methodPUT, methodDELETE
    , method, path, query, headers, body
    , pathOf, queryOf, headersOf
    , parseRequest, parseRequestResult, printRequest, link
    )
import Okapi.HTTP.Request.Body (IsoJson)
import Okapi.HTTP.Request.Method (DELETE, GET, KnownMethod (..), POST, PUT)
import Okapi.HTTP.Request.Path (segment, segment_, segments)
import Okapi.HTTP.Request.Query (ArrayStyle (..), param, param', param_, flag, flag', list, list')
import Okapi.HTTP.Request.Headers
    ( field, field', field_, contentType, cookie, cookie'
    , fieldRFC9651, fieldBareItem, fieldItem, fieldList, fieldDictionary
    )
import Okapi.HTTP.Response
    ( response, status200, status201, status204, status404, status500
    , parseResponse, parseResponseResult, printResponse
    )
import Okapi.HTTP.Response.Status (KnownStatus (..), S200, S201, S204, S404, S500)
import Okapi.HTTP.Responses (Cases, Responses, getResponses, cases, parseResponses, printResponses)
import Okapi.HTTP.Response.Attributes (attribute, attribute', secure, httpOnly)
