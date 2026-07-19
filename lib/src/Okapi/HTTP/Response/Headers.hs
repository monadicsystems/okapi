
-- | @'ResponseHeaders' = 'Headers' 'ForResponse'@ — re-exports the shared
--   'Okapi.HTTP.Headers' combinators (they're already free in which side
--   they're used on) plus the response-only 'setCookie' (see
--   "Okapi.HTTP.Request.Headers" for the request-only 'cookie'\/'cookie''
--   counterpart).
module Okapi.HTTP.Response.Headers (
    ResponseHeaders,
    ParseError (..),
    parser,
    printer,
    parseExact,
    raw,
    field,
    field',
    field_,
    fieldStruct,
    fieldBareItem,
    fieldItem,
    fieldList,
    fieldDict,
    contentType,
    setCookie,
    derived,
) where

import Okapi.HTTP.Headers
    ( Headers, ParseError (..), parser, printer, parseExact, raw
    , field, field', field_, fieldStruct, fieldBareItem, fieldItem, fieldList, fieldDict
    , contentType, setCookie, derived
    )
import Okapi.HTTP.Tree (ForResponse)

type ResponseHeaders = Headers ForResponse
