
-- | @'ResponseBody' = 'Body' 'ForResponse'@ — re-exports the shared
--   'Okapi.HTTP.Body' combinators (they're already free in which side
--   they're used on) — see 'Okapi.HTTP.Request.Body' for the request-only
--   'Okapi.HTTP.Request.Body.form' counterpart this side doesn't have.
module Okapi.HTTP.Response.Body (
    ResponseBody,
    None (..),
    IsoJson,
    ParseError (..),
    parser,
    printer,
    raw,
    json,
    jsonValue,
    none,
) where

import Okapi.HTTP.Body
    ( Body, None (..), IsoJson, ParseError (..), parser, printer, raw, json, jsonValue, none )
import Okapi.HTTP.Tree (ForResponse)

type ResponseBody = Body ForResponse
