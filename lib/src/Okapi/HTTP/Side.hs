
-- | Phantom markers distinguishing request-side from response-side codecs
--   (see "Okapi.HTTP.Headers", "Okapi.HTTP.Body") — never constructed, used
--   only as a type-level tag.
module Okapi.HTTP.Side (
    ForRequest,
    ForResponse,
) where

data ForRequest

data ForResponse
