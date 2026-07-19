{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE PatternSynonyms #-}

-- | The generic mode\/framework layer — contracts, endpoints, handles,
--   clients, links, and OpenAPI documents, sourced from "Okapi.Artifact.Endpoint",
--   "Okapi.Artifact.Client", "Okapi.Artifact.Link", "Okapi.Artifact.Function",
--   and "Okapi.Artifact.OpenApi": each of these consumes an 'HTTP' contract
--   and produces something else (a running server, a client function, a set
--   of hyperlinks, an OpenAPI document). Nothing here is
--   Request-or-Response-specific; for the actual HTTP DSL (@field@, @json@,
--   @seg@, method\/status singletons, and the 'Okapi.HTTP.Request.Request'\/
--   'Okapi.HTTP.Response.Response' types themselves), import
--   "Okapi.HTTP.Request" and\/or "Okapi.HTTP.Response" directly — each is a
--   complete, self-sufficient surface for its side.
--
--   'Okapi.Data.Request'\/'Okapi.Data.Response' (and the
--   'Okapi.Result.Request'\/'Okapi.Failure.Request' siblings alongside
--   them) are a third category, not exported from here either — the
--   decoded-value\/intermediate-parse\/accumulated-failure shapes that
--   "Okapi.HTTP.Request"\/"Okapi.HTTP.Response"'s own @parser@\/@printer@
--   operations produce and consume. They aren't DSL machinery like
--   'Okapi.HTTP.Tree' (nothing to author a codec with — no combinators of
--   their own) and they aren't artifacts built from an 'HTTP' contract like
--   the "Okapi.Artifact.*" modules — they're the value-level counterpart to
--   'Okapi.HTTP.Request.Request'\/'Okapi.HTTP.Response.Response'
--   themselves, so they sit at this same top level rather than nested
--   under either.
module Okapi
    (
      HTTP (..)
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
    ) where

import Okapi.HTTP (HTTP (..), Shape, Origin, AnyResponse, METHOD, PATH, QUERY, HEADERS, BODY, RESPOND, type (:&), Morph (..), morph)
import Okapi.Artifact.Function (Function, fn)
import Okapi.Artifact.Endpoint (Endpoint (..), endpoint, normalize, scope, type (~>), route, catchAll, Handle (..), handle, mount, run, toOpenApi, endpoints, Transformer (..), endpointsVia, handles)
import Okapi.Artifact.Client (Client, pattern Fn, ClientError (..), ClientSettings (..), fetch, clientFor, client, clientVia)
import Okapi.Artifact.Link (URI (..), Link (..), links, linksVia)
import Okapi.Artifact.OpenApi (contractToOpenApi, openApi, openApiVia)
import Okapi.HTTP.Tree
    ( SymTree
    , Leaf (..), Info (..), HasLeaf (..)
    , int, int16, int32, int64, integer
    , bool, float, double, scientific, text
    , day, localTime, utcTime, timeOfDay, uuid
    , (=.)
    )
import Okapi.HTTP.Body (IsoJson)
import Okapi.HTTP.Responses (Cases, Responses, getResponses, cases, parseResponses, printResponses)
