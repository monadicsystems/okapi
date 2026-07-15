
module Okapi.Mode.Contract (
    Shape,
    Contract (..),
    annotate,
    stripTags,
    collectTags,
) where

import Data.Kind (Type)
import Okapi.HTTP.Request qualified as HTTP
import Okapi.HTTP.Response qualified as HTTP
import Okapi.Record.Data qualified as Data
import Okapi.HTTP.Responses (Cases, Responses)
import Okapi.Tree (Tag)

data Shape
    (method  :: Type)
    (path    :: Type)
    (query   :: Type)
    (headers :: Type)
    (body    :: Type)
    (result  :: Type)

-- | A request paired with either a single response contract ('(:->)') or a
--   set of response alternatives ('(:-<)') — one full HTTP contract
--   description. 'Annotate' carries metadata (see 'Okapi.Tree.Tag') that
--   doesn't affect serving/dispatching at all — 'stripTags' peels it off
--   for consumers that don't care (every module here except
--   'Okapi.Artifact.OpenApi'); 'collectTags' reads it for the one that does.
data Contract shape where
    (:->) ::
        HTTP.Request method path query headers body ->
        HTTP.Response status resHeaders resBody ->
        Contract (Shape method path query headers body (Data.Response status resHeaders resBody))
    (:-<) ::
        Cases responses =>
        HTTP.Request method path query headers body ->
        Responses HTTP.Response responses ->
        Contract (Shape method path query headers body (responses Data.Response))
    Annotate :: [Tag] -> Contract shape -> Contract shape

-- | Attach metadata to a whole @req :-> res@ (or @req :-< resAlt@)
--   contract — never changes serving\/dispatching behavior, purely a
--   documentation\/introspection layer (consumed by 'Okapi.Artifact.OpenApi').
--   A separate function from 'Okapi.Tree.annotate' (which annotates one
--   'Okapi.Tree.Tree' node, not a whole contract) — qualify the import if
--   both are needed in the same file.
annotate :: [Tag] -> Contract shape -> Contract shape
annotate = Annotate

-- | Peel off every 'Annotate' layer, down to the real '(:->)'/'(:-<)'.
--   For consumers that only care about serving/dispatching, not metadata.
stripTags :: Contract shape -> Contract shape
stripTags (Annotate _ inner) = stripTags inner
stripTags f                  = f

-- | Collect every tag from any stacked 'annotate' calls, outermost first.
--   Chosen so nested 'annotate' calls are equivalent to one flattened
--   call: @annotate a $ annotate b $ x@ collects the same list as
--   @annotate (a <> b) x@ — outer-first order is what makes that hold.
--   For 'Okapi.Artifact.OpenApi', the one consumer that wants the metadata.
collectTags :: Contract shape -> [Tag]
collectTags (Annotate ts inner) = ts <> collectTags inner
collectTags _                   = []
