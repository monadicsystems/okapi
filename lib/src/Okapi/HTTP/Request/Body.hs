
-- | @'RequestBody' = 'Body' 'ForRequest'@ — re-exports the shared
--   'Okapi.HTTP.Body' combinators (they're already free in which side
--   they're used on) plus the request-only 'form' (see
--   'Okapi.HTTP.Response.Body' for the response side, which has no form
--   counterpart).
module Okapi.HTTP.Request.Body (
    RequestBody,
    None (..),
    IsoJson,
    ParseError (..),
    parser,
    printer,
    raw,
    json,
    jsonValue,
    form,
    none,
) where

import Okapi.HTTP.Body
    ( Body, None (..), IsoJson, ParseError (..), parser, printer, raw, json, jsonValue, form, none )
import Okapi.HTTP.Tree (ForRequest)

type RequestBody = Body ForRequest
