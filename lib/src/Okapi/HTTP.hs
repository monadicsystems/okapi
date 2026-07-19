
-- | The 'HTTP' contract type — a request paired with either a single
--   response ('(:->)') or a set of response alternatives ('(:-<)') — plus
--   the DSL operations genuinely shared between "Okapi.HTTP.Request" and
--   "Okapi.HTTP.Response" (free in the phantom 'Okapi.HTTP.Tree.ForRequest'\/
--   'Okapi.HTTP.Tree.ForResponse' tag, see
--   "Okapi.HTTP.Headers", "Okapi.HTTP.Body"). The type shares this
--   module's name the same way 'Okapi.HTTP.Request.Request' and
--   'Okapi.HTTP.Response.Response' do.
--
--   Note what's deliberately /not/ here: 'Okapi.Data.Response' (used
--   below in 'AnyResponse' and the '(:->)'\/'(:-<)' constructors) lives
--   at the bare top level, not under "Okapi.HTTP" — see "Okapi"'s own
--   haddock for why the decoded-value\/result\/failure shapes are a
--   third category, distinct from the DSL machinery this module and its
--   siblings provide.
module Okapi.HTTP (
    -- * The HTTP contract type
    Shape,
    HTTP (..),
    annotate,
    stripTags,
    collectTags,

    -- * Shape indexing
    Origin,
    AnyResponse,
    METHOD,
    PATH,
    QUERY,
    HEADERS,
    BODY,
    RESPOND,
    type (:&),
    Morph (..),
    morph,

    -- * Shared header combinators (free in the phantom Side tag)
    field,
    field',
    field_,
    contentType,
    fieldStruct,
    fieldBareItem,
    fieldItem,
    fieldList,
    fieldDict,
    MediaType (..),

    -- * Shared body combinators
    json,
    jsonValue,
    none,
    None (..),
    IsoJson,
) where

import Data.ByteString.Lazy qualified as LBS
import Data.Kind (Type)
import Data.Text (Text)
import Network.HTTP.Types qualified as Types
import Okapi.HTTP.Headers
    ( field, field', field_, contentType
    , fieldStruct, fieldBareItem, fieldItem, fieldList, fieldDict
    , MediaType (..)
    )
import Okapi.HTTP.Body (json, jsonValue, none, None (..), IsoJson)
import Okapi.HTTP.Request qualified as Req
import Okapi.HTTP.Response qualified as Res
import Okapi.HTTP.Responses (Cases, Responses)
import Okapi.HTTP.Tree (Tag)
import Okapi.Data.Response qualified as Data

data Shape
    (method  :: Type)
    (path    :: Type)
    (query   :: Type)
    (headers :: Type)
    (body    :: Type)
    (result  :: Type)

-- | A request paired with either a single response contract ('(:->)') or a
--   set of response alternatives ('(:-<)') — one full HTTP contract
--   description. 'Annotate' carries metadata (see 'Okapi.HTTP.Tree.Tag')
--   that doesn't affect serving/dispatching at all — 'stripTags' peels it
--   off for consumers that don't care (every module here except
--   "Okapi.Artifact.OpenApi"); 'collectTags' reads it for the one that does.
data HTTP shape where
    (:->) ::
        Req.Request method path query headers body ->
        Res.Response status resHeaders resBody ->
        HTTP (Shape method path query headers body (Data.Response status resHeaders resBody))
    (:-<) ::
        Cases responses =>
        Req.Request method path query headers body ->
        Responses Res.Response responses ->
        HTTP (Shape method path query headers body (responses Data.Response))
    Annotate :: [Tag] -> HTTP shape -> HTTP shape

-- | Attach metadata to a whole @req :-> res@ (or @req :-< resAlt@)
--   contract — never changes serving\/dispatching behavior, purely a
--   documentation\/introspection layer (consumed by "Okapi.Artifact.OpenApi").
--   A separate function from 'Okapi.HTTP.Tree.annotate' (which annotates
--   one 'Okapi.HTTP.Tree.Tree' node, not a whole contract) — qualify the
--   import if both are needed in the same file.
annotate :: [Tag] -> HTTP shape -> HTTP shape
annotate = Annotate

-- | Peel off every 'Annotate' layer, down to the real '(:->)'/'(:-<)'.
--   For consumers that only care about serving/dispatching, not metadata.
stripTags :: HTTP shape -> HTTP shape
stripTags (Annotate _ inner) = stripTags inner
stripTags f                  = f

-- | Collect every tag from any stacked 'annotate' calls, outermost first.
--   Chosen so nested 'annotate' calls are equivalent to one flattened
--   call: @annotate a $ annotate b $ x@ collects the same list as
--   @annotate (a <> b) x@ — outer-first order is what makes that hold.
--   For "Okapi.Artifact.OpenApi", the one consumer that wants the metadata.
collectTags :: HTTP shape -> [Tag]
collectTags (Annotate ts inner) = ts <> collectTags inner
collectTags _                   = []

-- | The fully generic response shape — status and headers left maximally
--   unconstrained, body fixed to the same raw default 'Origin' uses.
type AnyResponse = Data.Response Types.Status Types.ResponseHeaders (IO LBS.ByteString)

{- | The maximally unconstrained 'Shape' — exactly what
  @Okapi.HTTP.Request.any ':->' Okapi.HTTP.Response.any@ already
  infers today (the fully generic forms; deliberately not
  @get ':->' ok@, which each pin down one slot — @GET@,
  @KnownStatus 200@ — the opposite of what a maximally unconstrained
  starting point should do). Override individual slots with '(:&)':

> type MyShape = Origin :& METHOD GET :& PATH MyPath :& RESPOND MyResult
-}
type Origin =
    Shape
        Types.Method
        [Text]
        Types.Query
        Types.RequestHeaders
        (IO LBS.ByteString)
        AnyResponse

-- | Field tag for '(:&)' — overrides a 'Shape's @method@ slot. All-caps
--   rather than the bare slot name because @Method@\/@Path@\/@Query@\/
--   @Headers@\/@Body@ already name the Tree DSL's own GADTs elsewhere in
--   this library (e.g. "Okapi.HTTP.Request.Method") — a real collision,
--   not a style choice, since building an 'HTTP' contract already touches
--   the Tree DSL in the same file this would be used in. Case-sensitivity
--   is what keeps @METHOD@ from actually clashing with @Method@.
data METHOD (m :: Type)

-- | Field tag for '(:&)' — overrides a 'Shape's @path@ slot.
data PATH (p :: Type)

-- | Field tag for '(:&)' — overrides a 'Shape's @query@ slot.
data QUERY (q :: Type)

-- | Field tag for '(:&)' — overrides a 'Shape's @headers@ slot.
data HEADERS (h :: Type)

-- | Field tag for '(:&)' — overrides a 'Shape's @body@ slot.
data BODY (b :: Type)

{- | Field tag for '(:&)' — overrides a 'Shape's @result@ slot. Accepts
  either a single-response shape (@Data.Response status headers body@,
  matching '(:->)') or a multiple-responses shape (@responses Data.Response@
  for some @Cases responses@, matching '(:-<)') — '(:&)' doesn't constrain
  which, since either is just \"some type for this slot\" to it. Actual
  safety is enforced later, the same way any Haskell type signature is: a
  real 'HTTP' value only type-checks against a field if its own inferred
  shape actually matches.
-}
data RESPOND (r :: Type)

infixl 1 :&

type (:&) :: Type -> Type -> Type

-- | Type-level \"record update\" on a 'Shape' — @shape ':&' TAG y@ replaces
--   just that slot, leaving the rest untouched. Closed and dispatches
--   entirely on the outer constructor of the second argument, so the six
--   equations below could be reordered freely with no change in behavior.
type family (:&) shape field where
    (Shape _ p q h b r) :& METHOD m = Shape m p q h b r
    (Shape m _ q h b r) :& PATH p = Shape m p q h b r
    (Shape m p _ h b r) :& QUERY q = Shape m p q h b r
    (Shape m p q _ b r) :& HEADERS h = Shape m p q h b r
    (Shape m p q h _ r) :& BODY b = Shape m p q h b r
    (Shape m p q h b _) :& RESPOND r = Shape m p q h b r

{- | Lifts a shape-only, monad-agnostic type ('HTTP', 'Okapi.Artifact.Link.Link',
  'Okapi.Artifact.Client.Client') into the 2-arg slot a heterogeneous HKD
  record needs — @n@ is carried but never inspected, purely so each field
  of such a record can bake in its own @n@ the same way it already bakes
  in its own @shape@.
-}
newtype Morph (f :: Type -> Type) (n :: Type -> Type) shape = Morph (f shape)

morph :: f shape -> Morph f n shape
morph = Morph
