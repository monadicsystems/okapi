
-- | The 'HTTP' contract type — a request paired with either a single
--   response ('(:->)') or a set of response alternatives ('(:-<)') — and
--   the type-level machinery for indexing\/updating its 'Signature'. The
--   type shares this module's name the same way 'Okapi.HTTP.Request.Request'
--   and 'Okapi.HTTP.Response.Response' do.
--
--   Deliberately /not/ here: the genuinely side-free header\/body
--   combinators ('Okapi.HTTP.Headers.field', 'Okapi.HTTP.Body.json', etc.)
--   — reach those via "Okapi.HTTP.Headers"\/"Okapi.HTTP.Body" directly,
--   their true defining modules, not through a re-export here. Also not
--   here: 'Okapi.Response.Data' (used below in 'Base' and the
--   '(:->)'\/'(:-<)' constructors), which lives at the bare top level —
--   see "Okapi"'s own haddock for why the decoded-value\/result\/failure
--   shapes are a third category, distinct from the DSL machinery this
--   module and its siblings provide.
module Okapi.HTTP (
    -- * The HTTP contract type
    Signature,
    HTTP (..),
    annotate,
    stripTags,
    collectTags,

    -- * Signature indexing
    Base,
    METHOD,
    PATH,
    QUERY,
    HEADERS,
    BODY,
    STATUS,
    RESPONSES,
    type (:&),
    Morph (..),
    morph,
) where

import Data.Kind (Type)
import Okapi.HTTP.Headers qualified as Headers
import Okapi.HTTP.Body qualified as Body
import Okapi.HTTP.Request qualified as Req
import Okapi.HTTP.Request.Method qualified as Method
import Okapi.HTTP.Request.Path qualified as Path
import Okapi.HTTP.Request.Query qualified as Query
import Okapi.HTTP.Response qualified as Res
import Okapi.HTTP.Response.Status qualified as Status
import Okapi.HTTP.Responses (Responses, Responses')
import Okapi.HTTP.Tree (Tag)
import Okapi.Response.Data qualified as Data

data Signature
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
--   "Okapi.OpenApi"); 'collectTags' reads it for the one that does.
data HTTP shape where
    (:->) ::
        Req.Request method path query headers body ->
        Res.Response status resHeaders resBody ->
        HTTP (Signature method path query headers body (Data.Response status resHeaders resBody))
    (:-<) ::
        Responses responses =>
        Req.Request method path query headers body ->
        Responses' Res.Response responses ->
        HTTP (Signature method path query headers body (responses Data.Response))
    Annotate :: [Tag] -> HTTP shape -> HTTP shape

-- | Attach metadata to a whole @req :-> res@ (or @req :-< resAlt@)
--   contract — never changes serving\/dispatching behavior, purely a
--   documentation\/introspection layer (consumed by "Okapi.OpenApi").
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
--   For "Okapi.OpenApi", the one consumer that wants the metadata.
collectTags :: HTTP shape -> [Tag]
collectTags (Annotate ts inner) = ts <> collectTags inner
collectTags _                   = []

{- | The maximally unconstrained 'Signature' — exactly what
  @Okapi.HTTP.Request.any ':->' Okapi.HTTP.Response.any@ already
  infers today (the fully generic forms; deliberately not
  @get ':->' ok@, which each pin down one slot — @GET@,
  @KnownStatus 200@ — the opposite of what a maximally unconstrained
  starting point should do). Override individual slots with '(:&)':

> type MySignature = Base :& METHOD GET :& PATH MyPath :& RESPONSES MyResult
-}
type Base =
    Signature
        Method.Base
        Path.Base
        Query.Base
        Headers.Base
        Body.Base
        (Data.Response Status.Base Headers.Base Body.Base)

-- | Field tag for '(:&)' — overrides a 'Signature's @method@ slot (or a
--   bare 'Okapi.HTTP.Request.Request's). All-caps rather than the bare
--   slot name because @Method@\/@Path@\/@Query@\/@Headers@\/@Body@ already
--   name the Tree DSL's own GADTs elsewhere in this library (e.g.
--   "Okapi.HTTP.Request.Method") — a real collision, not a style choice,
--   since building an 'HTTP' contract already touches the Tree DSL in the
--   same file this would be used in. Case-sensitivity is what keeps
--   @METHOD@ from actually clashing with @Method@.
data METHOD (m :: Type)

-- | Field tag for '(:&)' — overrides a 'Signature's @path@ slot (or a
--   bare 'Okapi.HTTP.Request.Request's).
data PATH (p :: Type)

-- | Field tag for '(:&)' — overrides a 'Signature's @query@ slot (or a
--   bare 'Okapi.HTTP.Request.Request's).
data QUERY (q :: Type)

-- | Field tag for '(:&)' — overrides a 'Signature's, 'Okapi.HTTP.Request.Request's,
--   or 'Okapi.HTTP.Response.Response's @headers@ slot.
data HEADERS (h :: Type)

-- | Field tag for '(:&)' — overrides a 'Signature's, 'Okapi.HTTP.Request.Request's,
--   or 'Okapi.HTTP.Response.Response's @body@ slot.
data BODY (b :: Type)

-- | Field tag for '(:&)' — overrides a 'Okapi.HTTP.Response.Response's
--   @status@ slot.
data STATUS (s :: Type)

{- | Field tag for '(:&)' — overrides a 'Signature's @result@ slot. Accepts
  either a single-response shape (@Data.Response status headers body@,
  matching '(:->)') or a multiple-responses shape (@responses Data.Response@
  for some @Responses responses@, matching '(:-<)') — '(:&)' doesn't
  constrain which, since either is just \"some type for this slot\" to it.
  Actual safety is enforced later, the same way any Haskell type signature
  is: a real 'HTTP' value only type-checks against a field if its own
  inferred shape actually matches.
-}
data RESPONSES (r :: Type)

infixl 1 :&

type (:&) :: Type -> Type -> Type

{- | Type-level \"record update\", unified across 'Signature',
  'Okapi.HTTP.Request.Request', and 'Okapi.HTTP.Response.Response' —
  @target ':&' TAG y@ replaces just that slot, leaving the rest untouched.
  One closed family covering all three, since nothing about writing
  equations for a type family requires its left-hand sides to share a
  head constructor, only that whichever module defines it has every
  target type in scope — this module already does (it defines
  'Signature' itself and imports 'Okapi.HTTP.Request.Request'\/
  'Okapi.HTTP.Response.Response'). Dispatches entirely on the outer
  constructors of both arguments, so the equations below could be
  reordered freely with no change in behavior.
-}
type family (:&) target field where
    (Signature _ p q h b r) :& METHOD m = Signature m p q h b r
    (Signature m _ q h b r) :& PATH p = Signature m p q h b r
    (Signature m p _ h b r) :& QUERY q = Signature m p q h b r
    (Signature m p q _ b r) :& HEADERS h = Signature m p q h b r
    (Signature m p q h _ r) :& BODY b = Signature m p q h b r
    (Signature m p q h b _) :& RESPONSES r = Signature m p q h b r
    (Req.Request _ p q h b) :& METHOD m = Req.Request m p q h b
    (Req.Request m _ q h b) :& PATH p = Req.Request m p q h b
    (Req.Request m p _ h b) :& QUERY q = Req.Request m p q h b
    (Req.Request m p q _ b) :& HEADERS h = Req.Request m p q h b
    (Req.Request m p q h _) :& BODY b = Req.Request m p q h b
    (Res.Response _ h b) :& STATUS s = Res.Response s h b
    (Res.Response s _ b) :& HEADERS h = Res.Response s h b
    (Res.Response s h _) :& BODY b = Res.Response s h b

{- | Lifts a shape-only, monad-agnostic type ('HTTP', 'Okapi.Link.Link',
  'Okapi.Client.Client') into the 2-arg slot a heterogeneous HKD
  record needs — @n@ is carried but never inspected, purely so each field
  of such a record can bake in its own @n@ the same way it already bakes
  in its own @shape@.
-}
newtype Morph (f :: Type -> Type) (n :: Type -> Type) shape = Morph (f shape)

morph :: f shape -> Morph f n shape
morph = Morph
