{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE PatternSynonyms #-}

-- | The generic mode\/framework layer — contracts, servers, handles,
--   clients, links, and OpenAPI documents, sourced from "Okapi.Server",
--   "Okapi.Client", "Okapi.Link", "Okapi.Function", and "Okapi.OpenApi":
--   each of these consumes a 'Contract' and produces something else
--   (a running server, a client function, a set of hyperlinks, an OpenAPI
--   document). Nothing here is Request-or-Response-specific; for the
--   actual HTTP DSL (@field@, @json@, @seg@, method\/status singletons,
--   and the 'Okapi.HTTP.Request.Codec'\/'Okapi.HTTP.Response.Codec' types
--   themselves), import "Okapi.HTTP.Request" and\/or "Okapi.HTTP.Response"
--   directly — each is a complete, self-sufficient surface for its side.
--
--   That includes the decoded-value\/intermediate-parse\/accumulated-
--   failure shapes 'Okapi.HTTP.Request.Data'\/'Okapi.HTTP.Request.Result'\/
--   'Okapi.HTTP.Request.Failure' (and their 'Okapi.HTTP.Response' counterparts) —
--   these live alongside 'Okapi.HTTP.Request.Codec' in the same module now,
--   reachable through the same qualified import (e.g. @Req.Codec@,
--   @Req.Data@, @Req.Failure@, @Req.Result@ all via one @Req@ alias),
--   not re-exported from here either, for the same reason: they're the
--   value-level counterpart to the request\/response codec itself, not
--   DSL machinery like 'Okapi.Tree' or an artifact built from a
--   'Contract' like "Okapi.Server"\/"Okapi.Client"\/"Okapi.Link"\/
--   "Okapi.Function"\/"Okapi.OpenApi".
module Okapi
    (
      Contract (..)
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

import Okapi.Contract (Contract (..), Signature, Base, METHOD, PATH, QUERY, HEADERS, BODY, STATUS, RESPONSES, type (:&), annotate, stripTags, collectTags, Morph (..), morph)
import Okapi.Function (Function, fn)
import Okapi.Server (Server (..), server, normalize, scope, route, catchAll, Handle (..), handle, mount, run, toOpenApi, servers, serversVia, handles)
import Okapi.Transformer (type (~>), Transformer (..))
import Okapi.Client (Client, pattern Fn, ClientError (..), ClientSettings (..), fetch, clientFor, client, clientVia)
import Okapi.Link (URI (..), Link (..), links, linksVia)
import Okapi.OpenApi (contractToOpenApi, openApi, openApiVia)
import Okapi.Tree
    ( SymTree
    , Leaf (..), Info (..), HasLeaf (..)
    , int, int16, int32, int64, integer
    , bool, float, double, scientific, text
    , day, localTime, utcTime, timeOfDay, uuid
    , (=.)
    )
import Okapi.HTTP.Body (IsoJson)
import Okapi.HTTP.Responses (Responses, Responses', getResponses, responses, parseResponses, printResponses)
