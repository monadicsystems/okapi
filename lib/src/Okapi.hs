{-# LANGUAGE PatternSynonyms #-}

module Okapi
    (
      Forest (..)
    , Shape
    , fn
    , serve
    , tryServe
    , Server
    , Client
    , pattern Fn

    , Route (..)
    , Handle
    , handle
    , scope
    , tryHandle
    , dimapRoute
    , routes

    , server
    , client

    , openApi
    , endpointToOpenApi

    , type (~>)
    , fetch
    , clientFor
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

    , KnownStatus

    , ArrayStyle (..)

    , Cases
    , Responses
    , getResponses
    , cases

    , parseResponses
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

    , request, requestGET, requestPOST, requestPUT, requestDELETE
    , method, path, query, headers, body

    , response, response200, response201, response204, response404, response500

    , segment, segment_, segments
    , param, param', param_, flag, flag', list, list'

    , field, field', field_, contentType, cookie, cookie'
    , fieldRFC9651, fieldBareItem, fieldItem, fieldList, fieldDictionary

    , attribute, attribute', secure, httpOnly
    ) where

import Okapi.Mode.Forest (Forest (..), Shape)
import Okapi.Mode.Server (Server, fn, type (~>), serve, tryServe, server)
import Okapi.Mode.Route (Route (..), dimapRoute, routes)
import Okapi.Handle (Handle, handle, scope, tryHandle)
import Okapi.Mode.Client (Client, pattern Fn, ClientError (..), ClientSettings (..), fetch, clientFor, client)
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
    ( request, requestGET, requestPOST, requestPUT, requestDELETE
    , method, path, query, headers, body
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
    ( response, response200, response201, response204, response404, response500
    )
import Okapi.HTTP.Response.Status (KnownStatus)
import Okapi.HTTP.Responses (Cases, Responses, getResponses, cases, parseResponses, printResponses)
import Okapi.HTTP.Response.Attributes (attribute, attribute', secure, httpOnly)
