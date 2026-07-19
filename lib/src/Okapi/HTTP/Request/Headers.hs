
-- | @'RequestHeaders' = 'Headers' 'ForRequest'@ — re-exports the shared
--   'Okapi.HTTP.Headers' combinators (they're already free in which side
--   they're used on) plus the request-only 'cookie'\/'cookie'' (see
--   "Okapi.HTTP.Response.Headers" for the response-only 'setCookie'
--   counterpart).
module Okapi.HTTP.Request.Headers (
    RequestHeaders,
    ParseError (..),
    parser,
    printer,
    parseExact,
    coalesceCookies,
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
    cookie,
    cookie',
    derived,
) where

import Data.ByteString qualified as BS
import Data.List (partition)
import Network.HTTP.Types qualified as Types
import Okapi.HTTP.Headers
    ( Headers, ParseError (..), parser, printer, parseExact, raw
    , field, field', field_, fieldStruct, fieldBareItem, fieldItem, fieldList, fieldDict
    , contentType, cookie, cookie', derived
    )
import Okapi.HTTP.Tree (ForRequest)

type RequestHeaders = Headers ForRequest

-- | Merge multiple @cookie:@ headers into one, per RFC 6265 §5.4 — request
--   printing (unlike parsing) needs this explicitly since printers build
--   header lists one field at a time, so this is applied once at the end
--   by 'Okapi.HTTP.Request.printer' rather than inside 'printer' itself.
coalesceCookies :: [Types.Header] -> [Types.Header]
coalesceCookies hs =
    let (cks, rest) = partition ((== "cookie") . fst) hs
     in rest ++ [("cookie", BS.intercalate "; " (map snd cks)) | not (null cks)]
