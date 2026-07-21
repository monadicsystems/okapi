{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE PatternSynonyms #-}

-- | The generic mode\/framework layer — contracts, servers, handles,
--   clients, links, and OpenAPI documents, sourced from "Okapi.Server",
--   "Okapi.Client", "Okapi.Link", "Okapi.Function", and "Okapi.OpenApi":
--   each of these consumes an 'HTTP' contract and produces something else
--   (a running server, a client function, a set of hyperlinks, an OpenAPI
--   document). Nothing here is Request-or-Response-specific; for the
--   actual HTTP DSL (@field@, @json@, @seg@, method\/status singletons,
--   and the 'Okapi.HTTP.Request.Request'\/'Okapi.HTTP.Response.Response'
--   types themselves), import "Okapi.HTTP.Request" and\/or
--   "Okapi.HTTP.Response" directly — each is a complete, self-sufficient
--   surface for its side.
--
--   'Okapi.Request.Data'\/'Okapi.Response.Data' (and the
--   'Okapi.Request.Result'\/'Okapi.Request.Failure' siblings alongside
--   them) are a third category, not exported from here either — the
--   decoded-value\/intermediate-parse\/accumulated-failure shapes that
--   "Okapi.HTTP.Request"\/"Okapi.HTTP.Response"'s own @parser@\/@printer@
--   operations produce and consume. They aren't DSL machinery like
--   'Okapi.HTTP.Tree' (nothing to author a codec with — no combinators of
--   their own) and they aren't artifacts built from an 'HTTP' contract like
--   "Okapi.Server"\/"Okapi.Client"\/"Okapi.Link"\/"Okapi.Function"\/
--   "Okapi.OpenApi" — they're the value-level counterpart to
--   'Okapi.HTTP.Request.Request'\/'Okapi.HTTP.Response.Response'
--   themselves, so they sit at this same top level rather than nested
--   under either.
module Okapi
    (
      HTTP (..)
    , Signature
    , Base
    , METHOD
    , PATH
    , QUERY
    , HEADERS
    , BODY
    , STATUS
    , RESPONSES
    , type (:&)
    , annotate
    , stripTags
    , collectTags
    , fn
    , Function
    , Client
    , pattern Fn

    , Server (..)
    , server
    , normalize
    , scope
    , route
    , catchAll
    , Handle (..)
    , handle
    , mount
    , run
    , toOpenApi
    , servers
    , Transformer (..)
    , serversVia
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

    , Responses
    , Responses'
    , getResponses
    , responses

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

import Okapi.HTTP (HTTP (..), Signature, Base, METHOD, PATH, QUERY, HEADERS, BODY, STATUS, RESPONSES, type (:&), annotate, stripTags, collectTags, Morph (..), morph)
import Okapi.Function (Function, fn)
import Okapi.Server (Server (..), server, normalize, scope, route, catchAll, Handle (..), handle, mount, run, toOpenApi, servers, serversVia, handles)
import Okapi.Transformer (type (~>), Transformer (..))
import Okapi.Client (Client, pattern Fn, ClientError (..), ClientSettings (..), fetch, clientFor, client, clientVia)
import Okapi.Link (URI (..), Link (..), links, linksVia)
import Okapi.OpenApi (contractToOpenApi, openApi, openApiVia)
import Okapi.HTTP.Tree
    ( SymTree
    , Leaf (..), Info (..), HasLeaf (..)
    , int, int16, int32, int64, integer
    , bool, float, double, scientific, text
    , day, localTime, utcTime, timeOfDay, uuid
    , (=.)
    )
import Okapi.HTTP.Body (IsoJson)
import Okapi.HTTP.Responses (Responses, Responses', getResponses, responses, parseResponses, printResponses)
