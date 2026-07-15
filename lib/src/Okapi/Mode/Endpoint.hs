{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module Okapi.Mode.Endpoint (
    Endpoint (..),
    endpoint,
    normalize,
    scope,
    type (~>),
    route,
    catchAll,
    Handle (..),
    handle,
    mount,
    run,
    toOpenApi,
    GEndpoint,
    endpoints,
    Transformer (..),
    GEndpointVia,
    endpointsVia,
    GHandles,
    handles,
) where

import Data.Kind (Type)
import Data.OpenApi (OpenApi)
import GHC.Generics (
    C1,
    D1,
    Generic (..),
    K1 (..),
    M1 (..),
    Rec0,
    Rep,
    S1,
    (:*:) (..),
 )
import Network.HTTP.Types qualified as Types
import Network.Wai qualified as Wai
import Okapi.Artifact.OpenApi (contractToOpenApi)
import Okapi.HTTP.Request qualified as Request
import Okapi.HTTP.Response qualified as Response
import Okapi.HTTP.Responses qualified as Responses
import Okapi.Mode.Contract (Contract (..), Shape, stripTags)
import Okapi.Mode.Function (Function (..))
import Okapi.Mode.Morph (Morph (..))

data Endpoint (n :: Type -> Type) shape = Endpoint
    { transform :: n ~> IO
    , middleware :: Wai.Middleware
    , contract :: Contract shape
    , function :: Function n shape
    }

-- | Positional alternative to record syntax for building an 'Endpoint' —
--   @endpoint nt mw ct fn@ is the same value as @Endpoint { transform = nt,
--   middleware = mw, contract = ct, function = fn }@.
endpoint :: (n ~> IO) -> Wai.Middleware -> Contract shape -> Function n shape -> Endpoint n shape
endpoint = Endpoint

type (~>) :: (Type -> Type) -> (Type -> Type) -> Type
type f ~> g = forall a. f a -> g a

-- | Bake an 'Endpoint's 'transform' eagerly into its 'function', discarding
--   @n@ in favor of @IO@ — the same downgrade 'endpoints' applies to every
--   field internally, exposed here for one hand-built 'Endpoint' at a time.
normalize :: Endpoint n shape -> Endpoint IO shape
normalize (Endpoint nt mw ct (Function act)) = Endpoint id mw ct (Function (nt . act))

-- | Compose an extra middleware onto an 'Endpoint', wrapping whatever's
--   already there as the new outer layer. Combine with ordinary record
--   update to target one field of a generically-derived record of
--   endpoints without touching the rest:
--
-- > myEndpoints = base
-- >     { getUser    = scope authMw    base.getUser
-- >     , createUser = scope loggingMw base.createUser
-- >     }
scope :: Wai.Middleware -> Endpoint n shape -> Endpoint n shape
scope mw ep = ep{middleware = mw . middleware ep}

{- | Dispatch one bound route, wrapped in its own scoped middleware — carried
  on the 'Endpoint' itself via its 'middleware' field (pass @id@ there for
  none). On a non-match, @backup@ runs with the exact same, untouched
  request\/respond this call started with — the endpoint's own
  request\/response transformations never reach @backup@. Directly a
  'Wai.Middleware', so chaining several 'route' calls with plain @(.)@ gives
  each route a fresh pipeline, with no leakage between siblings, and the
  whole chain can be applied to a final fallback (e.g. 'catchAll') to get a
  runnable 'Wai.Application':

> app = route endpoint1
>     . route endpoint2
>     . route endpoint3
>     $ catchAll

  The same chain also works as an ordinary list, folded down with
  @('.')@ before being applied to the fallback:

> app =
>     foldr (.) id
>         [ route endpoint1
>         , route endpoint2
>         , route endpoint3
>         ]
>         $ catchAll

  Or fold with @('$')@ instead of @('.')@, with the fallback as the fold's
  base case directly — no trailing application needed, since @('$')@ just
  applies each 'route' to the accumulated 'Wai.Application' rather than
  composing functions:

> app =
>     foldr ($) catchAll
>         [ route endpoint1
>         , route endpoint2
>         , route endpoint3
>         ]

  Or, using @('&')@ from "Data.Function", build outward from the
  fallback — the /last/ 'route' applied ends up outermost, so it's the
  first one tried against an incoming request:

> app = catchAll
>     & route endpoint3
>     & route endpoint2
>     & route endpoint1

  For a list of routes you want to inspect later (e.g. to regenerate OpenAPI
  docs from whatever's actually mounted), see 'Handle'\/'handle'\/'mount'
  instead — this one only produces a 'Wai.Middleware', with no way back to
  the 'Endpoint' it came from.
-}
route :: Endpoint n shape -> Wai.Middleware
route (Endpoint runner mw contract (Function act)) backup waiReq respond =
    mw dispatch waiReq respond
  where
    dispatch :: Wai.Application
    dispatch waiReq' respond' = case stripTags contract of
        (req :-> singleRes) -> do
            parsed <- Request.parser req waiReq'
            case parsed of
                Left _ -> backup waiReq respond
                Right reqVal -> do
                    resVal <- runner (act (reqVal, waiReq'))
                    waiRes <- Response.printer singleRes resVal
                    respond' waiRes
        (req :-< resContracts) -> do
            parsed <- Request.parser req waiReq'
            case parsed of
                Left _ -> backup waiReq respond
                Right reqVal -> do
                    resVal <- runner (act (reqVal, waiReq'))
                    waiRes <- Responses.printResponses resContracts resVal
                    respond' waiRes
        Annotate _ _ -> error "unreachable: stripTags already peeled off every Annotate layer"

{- | Default fallback 'Wai.Application' — responds 404 to anything. Pass it as
  the final argument to a chain of 'route' calls if you don't have your
  own fallback.
-}
catchAll :: Wai.Application
catchAll _req respond = respond (Wai.responseLBS Types.status404 [] mempty)

{- | An 'Endpoint', existentially erased — @n@ and @shape@ vary route to
  route, so a plain @[Handle]@ is an ordinary, fully homogeneous list, even
  across routes running under different @n@ monads. Deliberately just an
  'Endpoint' with its types hidden, nothing more: each 'Endpoint' already
  carries its own middleware, so a whole @[Handle]@ can be mapped over with
  'mount' uniformly (e.g. @map mount handles@) while every entry still runs
  under its own scoped middleware — no parallel list of middlewares to keep
  in sync with @handles@. Build one with 'handle'; run it with 'mount';
  recover its 'Contract' for documentation with 'toOpenApi'.
-}
data Handle where
    Handle :: Endpoint n shape -> Handle

-- | Bundle an 'Endpoint' into a 'Handle' you can put in an ordinary list
--   alongside any other route.
handle :: Endpoint n shape -> Handle
handle = Handle

-- | Recover the same 'Wai.Middleware' 'route' would have produced directly —
--   so a list of 'Handle's and a chain built with 'route' mix freely via
--   @('.')@\/@('&')@.
mount :: Handle -> Wai.Middleware
mount (Handle ep) = route ep

-- | Fold a list of 'Handle's into one 'Wai.Middleware' via 'mount' — the
--   final runnable artifact:
--
-- > app = run [handle ep1, handle ep2] $ catchAll
run :: [Handle] -> Wai.Middleware
run = foldr (.) id . map mount

-- | Recover the 'Contract' inside a 'Handle' as an OpenAPI document —
--   ignores the middleware and handler entirely — so a whole @[Handle]@ can
--   be turned into docs the same way it's turned into an app via 'mount'.
toOpenApi :: Handle -> OpenApi
toOpenApi (Handle (Endpoint _ _ contract _)) = contractToOpenApi contract

class GEndpoint (n :: Type -> Type) (ctF :: Type -> Type) (fnF :: Type -> Type) (enF :: Type -> Type) where
    gEndpoint :: (n ~> IO) -> ctF () -> fnF () -> enF ()

instance (GEndpoint n ctF fnF enF) => GEndpoint n (D1 dm ctF) (D1 dm' fnF) (D1 dm'' enF) where
    gEndpoint nt (M1 ct) (M1 fn) = M1 (gEndpoint @n @ctF @fnF @enF nt ct fn)

instance (GEndpoint n ctF fnF enF) => GEndpoint n (C1 cm ctF) (C1 cm' fnF) (C1 cm'' enF) where
    gEndpoint nt (M1 ct) (M1 fn) = M1 (gEndpoint @n @ctF @fnF @enF nt ct fn)

instance
    (GEndpoint n ctL fnL enL, GEndpoint n ctR fnR enR) =>
    GEndpoint n (ctL :*: ctR) (fnL :*: fnR) (enL :*: enR)
    where
    gEndpoint nt (ctL :*: ctR) (fnL :*: fnR) =
        gEndpoint @n @ctL @fnL @enL nt ctL fnL :*: gEndpoint @n @ctR @fnR @enR nt ctR fnR

instance
    GEndpoint
        n
        (S1 sm (Rec0 (Contract (Shape method path query headers body result))))
        (S1 sm' (Rec0 (Function n (Shape method path query headers body result))))
        (S1 sm'' (Rec0 (Endpoint IO (Shape method path query headers body result))))
    where
    gEndpoint nt (M1 (K1 ct)) (M1 (K1 (Function act))) = M1 (K1 (Endpoint id id ct (Function (nt . act))))

{- | Lets a field be a nested record of the same shape (@moreRoutes ::
  MoreRoutes f@) instead of a concrete 'Contract'\/'Function'\/'Endpoint' —
  recurses via 'endpoints' itself. Never overlaps the instance above: its
  leaf pattern is @nested Contract@ (some record applied to 'Contract'
  itself, kind @Type -> Type@) versus @Contract (Shape ...)@ ('Contract'
  applied to a concrete 'Shape', kind @Type@) — unifying the two would
  require @Contract@ to have both kinds at once, which can't happen.
-}
instance
    ( Generic (nested Contract)
    , Generic (nested (Function n))
    , Generic (nested (Endpoint IO))
    , GEndpoint n (Rep (nested Contract)) (Rep (nested (Function n))) (Rep (nested (Endpoint IO)))
    ) =>
    GEndpoint
        n
        (S1 sm (Rec0 (nested Contract)))
        (S1 sm' (Rec0 (nested (Function n))))
        (S1 sm'' (Rec0 (nested (Endpoint IO))))
    where
    gEndpoint nt (M1 (K1 ctVal)) (M1 (K1 fnVal)) = M1 (K1 (endpoints nt ctVal fnVal))

{- | Builds every field's 'Endpoint' from a shared @nt@, normalizing each one
  down to 'Endpoint' 'IO' in the process (see 'normalize') — @n@ never
  appears in the result. Every field gets @id@ for its 'middleware'; give
  individual fields their own with 'scope' plus ordinary record update on
  the result.
-}
endpoints ::
    forall record n.
    ( Generic (record Contract)
    , Generic (record (Function n))
    , Generic (record (Endpoint IO))
    , GEndpoint n (Rep (record Contract)) (Rep (record (Function n))) (Rep (record (Endpoint IO)))
    ) =>
    (n ~> IO) ->
    record Contract ->
    record (Function n) ->
    record (Endpoint IO)
endpoints nt contracts handlers =
    to (gEndpoint @n @(Rep (record Contract)) @(Rep (record (Function n))) nt (from contracts) (from handlers))

newtype Transformer n shape = Transformer (n ~> IO)

class GEndpointVia (trF :: Type -> Type) (ctF :: Type -> Type) (fnF :: Type -> Type) (enF :: Type -> Type) where
    gEndpointVia :: trF () -> ctF () -> fnF () -> enF ()

instance (GEndpointVia trF ctF fnF enF) => GEndpointVia (D1 dm trF) (D1 dm' ctF) (D1 dm'' fnF) (D1 dm''' enF) where
    gEndpointVia (M1 tr) (M1 ct) (M1 fn) = M1 (gEndpointVia @trF @ctF @fnF @enF tr ct fn)

instance (GEndpointVia trF ctF fnF enF) => GEndpointVia (C1 cm trF) (C1 cm' ctF) (C1 cm'' fnF) (C1 cm''' enF) where
    gEndpointVia (M1 tr) (M1 ct) (M1 fn) = M1 (gEndpointVia @trF @ctF @fnF @enF tr ct fn)

instance
    (GEndpointVia trL ctL fnL enL, GEndpointVia trR ctR fnR enR) =>
    GEndpointVia (trL :*: trR) (ctL :*: ctR) (fnL :*: fnR) (enL :*: enR)
    where
    gEndpointVia (trL :*: trR) (ctL :*: ctR) (fnL :*: fnR) =
        gEndpointVia @trL @ctL @fnL @enL trL ctL fnL :*: gEndpointVia @trR @ctR @fnR @enR trR ctR fnR

instance
    GEndpointVia
        (S1 sm (Rec0 (Transformer n (Shape method path query headers body result))))
        (S1 sm' (Rec0 (Morph Contract n (Shape method path query headers body result))))
        (S1 sm'' (Rec0 (Function n (Shape method path query headers body result))))
        (S1 sm''' (Rec0 (Endpoint n (Shape method path query headers body result))))
    where
    gEndpointVia (M1 (K1 (Transformer nt))) (M1 (K1 (Morph ct))) (M1 (K1 fn)) =
        M1 (K1 (Endpoint nt id ct fn))

{- | Lets a field be a nested record of the same shape instead of a
  concrete 'Transformer'\/'Morph' 'Contract'\/'Function' triple — recurses
  via 'endpointsVia' itself. Same non-overlap argument as 'GEndpoint's
  nested instance: the leaf pattern here is @nested Transformer@ etc.
  (some record applied to this pipeline's own functors), never coinciding
  with the concrete instance above (@Transformer n (Shape ...)@ is
  @Transformer@ applied to two args of kind @Type -> Type@ then @Type@; the
  nested pattern applies some @nested@ to @Transformer@ itself, kind
  @(Type -> Type) -> Type -> Type@ — the two can't unify).
-}
instance
    ( Generic (nested Transformer)
    , Generic (nested (Morph Contract))
    , Generic (nested Function)
    , Generic (nested Endpoint)
    , GEndpointVia (Rep (nested Transformer)) (Rep (nested (Morph Contract))) (Rep (nested Function)) (Rep (nested Endpoint))
    ) =>
    GEndpointVia
        (S1 sm    (Rec0 (nested Transformer)))
        (S1 sm'   (Rec0 (nested (Morph Contract))))
        (S1 sm''  (Rec0 (nested Function)))
        (S1 sm''' (Rec0 (nested Endpoint)))
    where
    gEndpointVia (M1 (K1 trVal)) (M1 (K1 ctVal)) (M1 (K1 fnVal)) =
        M1 (K1 (endpointsVia trVal ctVal fnVal))

{- | Heterogeneous-@n@ counterpart to 'endpoints': instead of one shared @nt@
  for the whole record, each field supplies its own natural transformation
  via 'Transformer', its own contract via 'Morph' (lifting a plain
  'Contract' — see 'Okapi.Mode.Morph.morph'), and its own handler via
  'Function' — with @n@ free to differ field to field, exactly the way
  @shape@ already does. Argument order mirrors 'endpoint': transform-like
  thing first, then contract, then function.

  Unlike 'endpoints', the result stays @record Endpoint@ rather than
  @record (Endpoint IO)@ — each field's @n@ is baked directly into the
  record's own field declarations (@getUser :: f App1 Shape1@, etc.), so
  there's no single @n@ left to normalize away, and none of
  'route'\/'scope'\/'mount'\/'handle' need one anyway.
-}
endpointsVia ::
    forall record.
    ( Generic (record Transformer)
    , Generic (record (Morph Contract))
    , Generic (record Function)
    , Generic (record Endpoint)
    , GEndpointVia (Rep (record Transformer)) (Rep (record (Morph Contract))) (Rep (record Function)) (Rep (record Endpoint))
    ) =>
    record Transformer ->
    record (Morph Contract) ->
    record Function ->
    record Endpoint
endpointsVia transforms contracts handlers =
    to (gEndpointVia @(Rep (record Transformer)) @(Rep (record (Morph Contract))) @(Rep (record Function)) (from transforms) (from contracts) (from handlers))

class GHandles (enF :: Type -> Type) where
    gHandles :: enF () -> [Handle]

instance (GHandles enF) => GHandles (D1 dm enF) where
    gHandles (M1 en) = gHandles en

instance (GHandles enF) => GHandles (C1 cm enF) where
    gHandles (M1 en) = gHandles en

instance (GHandles enL, GHandles enR) => GHandles (enL :*: enR) where
    gHandles (enL :*: enR) = gHandles enL <> gHandles enR

instance GHandles (S1 sm (Rec0 (Endpoint n shape))) where
    gHandles (M1 (K1 ep)) = [handle ep]

{- | Lets a field be a nested record of 'Endpoint's (@moreRoutes ::
  MoreRoutes Endpoint@) instead of one concrete 'Endpoint' — recurses via
  'handles' itself. The pattern nests 'Endpoint' bare (the same functor
  'endpoints'\/'endpointsVia' already produce), not "any 'Generic' type" —
  that's what keeps this from risking overlap with the instance above even
  if 'Endpoint' itself later derives 'Generic' for unrelated reasons.
-}
instance
    ( Generic (nested Endpoint)
    , GHandles (Rep (nested Endpoint))
    ) =>
    GHandles (S1 sm (Rec0 (nested Endpoint)))
    where
    gHandles (M1 (K1 epVal)) = handles epVal

{- | Same as above, but for a nested record built via 'endpoints' rather
  than 'endpointsVia' — there, the functor is @Endpoint n@ (1-arg, @n@
  already fixed by whichever record produced it — @IO@ for 'endpoints'
  specifically, but left free here since nothing about collapsing to
  'Handle' cares), not bare 'Endpoint' (2-arg). The two instances never
  overlap: @nested@'s own kind already differs between them
  (@((Type -> Type) -> Type -> Type) -> Type@ vs @(Type -> Type) -> Type@),
  before even considering that 'Endpoint' (unapplied) and @Endpoint n@
  (applied to a monad) are different types outright.
-}
instance
    ( Generic (nested (Endpoint n))
    , GHandles (Rep (nested (Endpoint n)))
    ) =>
    GHandles (S1 sm (Rec0 (nested (Endpoint n))))
    where
    gHandles (M1 (K1 epVal)) = handles epVal

{- | Collapse every field of a record of 'Endpoint's into the foundational
  @[Handle]@ representation — works on the output of both 'endpoints'
  (@record (Endpoint IO)@) and 'endpointsVia' (@record Endpoint@), since
  this only looks at each field's own 'Endpoint' value via 'Generic', not
  at the record's own kind. Mixes freely with hand-built 'Handle's:

> handles myEndpoints ++ [handle oneOffEndpoint]

  From here, everything is the same ordinary list code the foundational
  mechanism already uses: 'run' for the app, @foldMap 'toOpenApi'@ for docs
  derived from a fully-built record of endpoints (contract-only docs
  without needing handlers built at all still go through
  'Okapi.Artifact.OpenApi.openApiVia' directly on the contracts record
  instead).
-}
handles :: (Generic r, GHandles (Rep r)) => r -> [Handle]
handles = gHandles . from
