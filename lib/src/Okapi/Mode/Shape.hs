module Okapi.Mode.Shape (
    Origin,
    AnyResponse,
    METHOD,
    PATH,
    QUERY,
    HEADERS,
    BODY,
    RESPOND,
    type (:&),
) where

import Data.ByteString.Lazy qualified as LBS
import Data.Kind (Type)
import Data.Text (Text)
import Network.HTTP.Types qualified as Types
import Okapi.Mode.Contract (Shape)
import Okapi.Record.Data qualified as Data

-- | The fully generic response shape — status and headers left maximally
--   unconstrained, body fixed to the same raw default 'Origin' uses.
type AnyResponse = Data.Response Types.Status Types.ResponseHeaders (IO LBS.ByteString)

{- | The maximally unconstrained 'Shape' — exactly what
  @Okapi.HTTP.Request.req ':->' Okapi.HTTP.Response.res@ already
  infers today (the fully generic forms; deliberately not
  @reqGET ':->' res200@, which each pin down one slot — @GET@,
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
--   not a style choice, since building a 'Okapi.Mode.Contract.Contract'
--   already touches the Tree DSL in the same file this would be used in.
--   Case-sensitivity is what keeps @METHOD@ from actually clashing with
--   @Method@.
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
  real 'Okapi.Mode.Contract.Contract' value only type-checks against a
  field if its own inferred shape actually matches.
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
