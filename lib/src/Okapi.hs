{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE PatternSynonyms #-}

module Okapi
    (
      Contract (..)
    , Shape
    , Origin
    , AnyResponse
    , METHOD
    , PATH
    , QUERY
    , HEADERS
    , BODY
    , RESPOND
    , type (:&)
    , fn
    , Function
    , Client
    , pattern Fn

    , Endpoint (..)
    , endpoint
    , normalize
    , scope
    , route
    , catchAll
    , Handle (..)
    , handle
    , mount
    , run
    , toOpenApi
    , endpoints
    , Transformer (..)
    , endpointsVia
    , handles

    , Morph (..)
    , morph

    , client
    , clientVia

    , openApi
    , contractToOpenApi
    , openApiVia

    , type (~>)
    , fetch
    , clientFor
    , ClientError (..)
    , ClientSettings (..)

    , URI (..)
    , Link (..)
    , links
    , linksVia

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

    , Request (..)
    , req, reqGET, reqPOST, reqPUT, reqDELETE
    , method, path, query

    , Response (..)
    , res, res200, res201, res204, res404, res500

    , seg, seg_, lit, segs
    , param, param', param_, flag, flag', list, list'

    , attr, attr', secure, httpOnly

    -- Header combinators. field/field'/field_/contentType/fieldStruct/
    -- fieldBareItem/fieldItem/fieldList/fieldDict are one shared definition
    -- (free in the phantom ForRequest/ForResponse tag from "Okapi.HTTP.Side")
    -- -- they work unqualified in either a Request or a Response headers
    -- block, resolved by ordinary type inference from context. cookie/
    -- cookie' and setCookie are genuinely side-pinned (different names, no
    -- collision either way), so they ride along here too.
    , field, field', field_, contentType
    , fieldStruct, fieldBareItem, fieldItem, fieldList, fieldDict
    , cookie, cookie'
    , setCookie
    , MediaType (..)

    -- Body combinators. json/jsonValue/noContent are shared the same way;
    -- form is request-only (pinned to Body ForRequest at its constructor).
    , json, jsonValue, form, noContent
    , None (..)
    ) where

import Okapi.Mode.Contract (Contract (..), Shape)
import Okapi.Mode.Shape (Origin, AnyResponse, METHOD, PATH, QUERY, HEADERS, BODY, RESPOND, type (:&))
import Okapi.Mode.Function (Function, fn)
import Okapi.Mode.Morph (Morph (..), morph)
import Okapi.Mode.Endpoint (Endpoint (..), endpoint, normalize, scope, type (~>), route, catchAll, Handle (..), handle, mount, run, toOpenApi, endpoints, Transformer (..), endpointsVia, handles)
import Okapi.Mode.Client (Client, pattern Fn, ClientError (..), ClientSettings (..), fetch, clientFor, client, clientVia)
import Okapi.Mode.Link (URI (..), Link (..), links, linksVia)
import Okapi.Artifact.OpenApi (contractToOpenApi, openApi, openApiVia)
import Okapi.Tree
    ( SymTree
    , Leaf (..), Info (..), HasLeaf (..)
    , int, int16, int32, int64, integer
    , bool, float, double, scientific, text
    , day, localTime, utcTime, timeOfDay, uuid
    , (=.)
    )
import Okapi.HTTP.Request
    ( Request (..)
    , req, reqGET, reqPOST, reqPUT, reqDELETE
    , method, path, query
    )
import Okapi.HTTP.Body (IsoJson, json, jsonValue, noContent, None (..))
import Okapi.HTTP.Request.Method (DELETE, GET, KnownMethod (..), POST, PUT)
import Okapi.HTTP.Request.Path (seg, seg_, lit, segs)
import Okapi.HTTP.Request.Query (ArrayStyle (..), param, param', param_, flag, flag', list, list')
import Okapi.HTTP.Request.Body (form)
import Okapi.HTTP.Request.Headers (cookie, cookie')
import Okapi.HTTP.Response
    ( Response (..)
    , res, res200, res201, res204, res404, res500
    )
import Okapi.HTTP.Response.Headers (setCookie)
import Okapi.HTTP.Response.Status (KnownStatus (..), S200, S201, S204, S404, S500)
import Okapi.HTTP.Responses (Cases, Responses, getResponses, cases, parseResponses, printResponses)
import Okapi.HTTP.Headers
    ( field, field', field_, contentType
    , fieldStruct, fieldBareItem, fieldItem, fieldList, fieldDict
    , MediaType (..)
    )
import Okapi.HTTP.Headers.Attributes (attr, attr', secure, httpOnly)
