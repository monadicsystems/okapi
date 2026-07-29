{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module Okapi.Server (
    Server (..),
    server,
    normalize,
    scope,
    route,
    catchAll,
    Handle (..),
    handle,
    mount,
    mountAll,
    toOpenApi,
    GServer,
    servers,
    GServerVia,
    serversVia,
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
import Okapi.OpenApi (contractToOpenApi)
import Okapi.HTTP.Request qualified as Request
import Okapi.HTTP.Response qualified as Response
import Okapi.HTTP.Responses qualified as Responses
import Okapi.Contract (Contract (..), Signature, stripTags, Morph (..))
import Okapi.Function (Function (..))
import Okapi.Transformer (type (~>), Transformer (..))

data Server (n :: Type -> Type) shape = Server
    { transform :: n ~> IO
    , middleware :: Wai.Middleware
    , contract :: Contract shape
    , function :: Function n shape
    }

-- | Positional alternative to record syntax for building a 'Server' —
--   @server nt mw ct fn@ is the same value as @Server { transform = nt,
--   middleware = mw, contract = ct, function = fn }@.
server :: (n ~> IO) -> Wai.Middleware -> Contract shape -> Function n shape -> Server n shape
server = Server

-- | Bake a 'Server's 'transform' eagerly into its 'function', discarding
--   @n@ in favor of @IO@ — the same downgrade 'servers' applies to every
--   field internally, exposed here for one hand-built 'Server' at a time.
normalize :: Server n shape -> Server IO shape
normalize (Server nt mw ct (Function act)) = Server id mw ct (Function (nt . act))

-- | Compose an extra middleware onto a 'Server', wrapping whatever's
--   already there as the new outer layer. Combine with ordinary record
--   update to target one field of a generically-derived record of
--   servers without touching the rest:
--
-- > myServers = base
-- >     { getUser    = scope authMw    base.getUser
-- >     , createUser = scope loggingMw base.createUser
-- >     }
scope :: Wai.Middleware -> Server n shape -> Server n shape
scope mw ep = ep{middleware = mw . middleware ep}

{- | Dispatch one bound route, wrapped in its own scoped middleware — carried
  on the 'Server' itself via its 'middleware' field (pass @id@ there for
  none). On a non-match, @backup@ runs with the exact same, untouched
  request\/respond this call started with — the server's own
  request\/response transformations never reach @backup@. Directly a
  'Wai.Middleware', so chaining several 'route' calls with plain @(.)@ gives
  each route a fresh pipeline, with no leakage between siblings, and the
  whole chain can be applied to a final fallback (e.g. 'catchAll') to get a
  runnable 'Wai.Application':

> app = route server1
>     . route server2
>     . route server3
>     $ catchAll

  The same chain also works as an ordinary list, folded down with
  @('.')@ before being applied to the fallback:

> app =
>     foldr (.) id
>         [ route server1
>         , route server2
>         , route server3
>         ]
>         $ catchAll

  Or fold with @('$')@ instead of @('.')@, with the fallback as the fold's
  base case directly — no trailing application needed, since @('$')@ just
  applies each 'route' to the accumulated 'Wai.Application' rather than
  composing functions:

> app =
>     foldr ($) catchAll
>         [ route server1
>         , route server2
>         , route server3
>         ]

  Or, using @('&')@ from "Data.Function", build outward from the
  fallback — the /last/ 'route' applied ends up outermost, so it's the
  first one tried against an incoming request:

> app = catchAll
>     & route server3
>     & route server2
>     & route server1

  For a list of routes you want to inspect later (e.g. to regenerate OpenAPI
  docs from whatever's actually mounted), see 'Handle'\/'handle'\/'mount'
  instead — this one only produces a 'Wai.Middleware', with no way back to
  the 'Server' it came from.
-}
route :: Server n shape -> Wai.Middleware
route (Server runner mw contract (Function act)) backup waiReq respond =
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

{- | A 'Server', existentially erased — @n@ and @shape@ vary route to
  route, so a plain @[Handle]@ is an ordinary, fully homogeneous list, even
  across routes running under different @n@ monads. Deliberately just a
  'Server' with its types hidden, nothing more: each 'Server' already
  carries its own middleware, so a whole @[Handle]@ can be mapped over with
  'mount' uniformly (e.g. @map mount handles@) while every entry still runs
  under its own scoped middleware — no parallel list of middlewares to keep
  in sync with @handles@. Build one with 'handle'; run it with 'mount';
  recover its 'Contract' for documentation with 'toOpenApi'.
-}
data Handle where
    Handle :: Server n shape -> Handle

-- | Bundle a 'Server' into a 'Handle' you can put in an ordinary list
--   alongside any other route.
handle :: Server n shape -> Handle
handle = Handle

-- | Recover the same 'Wai.Middleware' 'route' would have produced directly —
--   so a list of 'Handle's and a chain built with 'route' mix freely via
--   @('.')@\/@('&')@.
mount :: Handle -> Wai.Middleware
mount (Handle ep) = route ep

-- | Fold a list of 'Handle's into one 'Wai.Middleware' via 'mount' — the
--   final runnable artifact. Named to pair with 'mount' the way 'handles'
--   pairs with 'handle' — 'mount' is one, 'mountAll' is the whole list —
--   and deliberately not @run@, to avoid colliding with e.g. @Warp.run@
--   wherever both appear in the same example:
--
-- > app = mountAll [handle ep1, handle ep2] $ catchAll
mountAll :: [Handle] -> Wai.Middleware
mountAll = foldr (.) id . map mount

-- | Recover the 'Contract' inside a 'Handle' as an OpenAPI document —
--   ignores the middleware and handler entirely — so a whole @[Handle]@ can
--   be turned into docs the same way it's turned into an app via 'mount'.
toOpenApi :: Handle -> OpenApi
toOpenApi (Handle (Server _ _ contract _)) = contractToOpenApi contract

class GServer (n :: Type -> Type) (ctF :: Type -> Type) (fnF :: Type -> Type) (enF :: Type -> Type) where
    gServer :: (n ~> IO) -> ctF () -> fnF () -> enF ()

instance (GServer n ctF fnF enF) => GServer n (D1 dm ctF) (D1 dm' fnF) (D1 dm'' enF) where
    gServer nt (M1 ct) (M1 fn) = M1 (gServer @n @ctF @fnF @enF nt ct fn)

instance (GServer n ctF fnF enF) => GServer n (C1 cm ctF) (C1 cm' fnF) (C1 cm'' enF) where
    gServer nt (M1 ct) (M1 fn) = M1 (gServer @n @ctF @fnF @enF nt ct fn)

instance
    (GServer n ctL fnL enL, GServer n ctR fnR enR) =>
    GServer n (ctL :*: ctR) (fnL :*: fnR) (enL :*: enR)
    where
    gServer nt (ctL :*: ctR) (fnL :*: fnR) =
        gServer @n @ctL @fnL @enL nt ctL fnL :*: gServer @n @ctR @fnR @enR nt ctR fnR

instance
    GServer
        n
        (S1 sm (Rec0 (Contract (Signature method path query headers body result))))
        (S1 sm' (Rec0 (Function n (Signature method path query headers body result))))
        (S1 sm'' (Rec0 (Server IO (Signature method path query headers body result))))
    where
    gServer nt (M1 (K1 ct)) (M1 (K1 (Function act))) = M1 (K1 (Server id id ct (Function (nt . act))))

{- | Lets a field be a nested record of the same shape (@moreRoutes ::
  MoreRoutes f@) instead of a concrete 'Contract'\/'Function'\/'Server' —
  recurses via 'servers' itself. Never overlaps the instance above: its
  leaf pattern is @nested Contract@ (some record applied to 'Contract'
  itself, kind @Type -> Type@) versus @Contract (Signature ...)@ ('Contract'
  applied to a concrete 'Signature', kind @Type@) — unifying the two would
  require @Contract@ to have both kinds at once, which can't happen.
-}
instance
    ( Generic (nested Contract)
    , Generic (nested (Function n))
    , Generic (nested (Server IO))
    , GServer n (Rep (nested Contract)) (Rep (nested (Function n))) (Rep (nested (Server IO)))
    ) =>
    GServer
        n
        (S1 sm (Rec0 (nested Contract)))
        (S1 sm' (Rec0 (nested (Function n))))
        (S1 sm'' (Rec0 (nested (Server IO))))
    where
    gServer nt (M1 (K1 ctVal)) (M1 (K1 fnVal)) = M1 (K1 (servers nt ctVal fnVal))

{- | Builds every field's 'Server' from a shared @nt@, normalizing each one
  down to 'Server' 'IO' in the process (see 'normalize') — @n@ never
  appears in the result. Every field gets @id@ for its 'middleware'; give
  individual fields their own with 'scope' plus ordinary record update on
  the result.
-}
servers ::
    forall record n.
    ( Generic (record Contract)
    , Generic (record (Function n))
    , Generic (record (Server IO))
    , GServer n (Rep (record Contract)) (Rep (record (Function n))) (Rep (record (Server IO)))
    ) =>
    (n ~> IO) ->
    record Contract ->
    record (Function n) ->
    record (Server IO)
servers nt contracts handlers =
    to (gServer @n @(Rep (record Contract)) @(Rep (record (Function n))) nt (from contracts) (from handlers))

class GServerVia (trF :: Type -> Type) (ctF :: Type -> Type) (fnF :: Type -> Type) (enF :: Type -> Type) where
    gServerVia :: trF () -> ctF () -> fnF () -> enF ()

instance (GServerVia trF ctF fnF enF) => GServerVia (D1 dm trF) (D1 dm' ctF) (D1 dm'' fnF) (D1 dm''' enF) where
    gServerVia (M1 tr) (M1 ct) (M1 fn) = M1 (gServerVia @trF @ctF @fnF @enF tr ct fn)

instance (GServerVia trF ctF fnF enF) => GServerVia (C1 cm trF) (C1 cm' ctF) (C1 cm'' fnF) (C1 cm''' enF) where
    gServerVia (M1 tr) (M1 ct) (M1 fn) = M1 (gServerVia @trF @ctF @fnF @enF tr ct fn)

instance
    (GServerVia trL ctL fnL enL, GServerVia trR ctR fnR enR) =>
    GServerVia (trL :*: trR) (ctL :*: ctR) (fnL :*: fnR) (enL :*: enR)
    where
    gServerVia (trL :*: trR) (ctL :*: ctR) (fnL :*: fnR) =
        gServerVia @trL @ctL @fnL @enL trL ctL fnL :*: gServerVia @trR @ctR @fnR @enR trR ctR fnR

instance
    GServerVia
        (S1 sm (Rec0 (Transformer n (Signature method path query headers body result))))
        (S1 sm' (Rec0 (Morph Contract n (Signature method path query headers body result))))
        (S1 sm'' (Rec0 (Function n (Signature method path query headers body result))))
        (S1 sm''' (Rec0 (Server n (Signature method path query headers body result))))
    where
    gServerVia (M1 (K1 (Transformer nt))) (M1 (K1 (Morph ct))) (M1 (K1 fn)) =
        M1 (K1 (Server nt id ct fn))

{- | Lets a field be a nested record of the same shape instead of a
  concrete 'Transformer'\/'Morph' 'Contract'\/'Function' triple — recurses
  via 'serversVia' itself. Same non-overlap argument as 'GServer's
  nested instance: the leaf pattern here is @nested Transformer@ etc.
  (some record applied to this pipeline's own functors), never coinciding
  with the concrete instance above (@Transformer n (Signature ...)@ is
  @Transformer@ applied to two args of kind @Type -> Type@ then @Type@; the
  nested pattern applies some @nested@ to @Transformer@ itself, kind
  @(Type -> Type) -> Type -> Type@ — the two can't unify).
-}
instance
    ( Generic (nested Transformer)
    , Generic (nested (Morph Contract))
    , Generic (nested Function)
    , Generic (nested Server)
    , GServerVia (Rep (nested Transformer)) (Rep (nested (Morph Contract))) (Rep (nested Function)) (Rep (nested Server))
    ) =>
    GServerVia
        (S1 sm    (Rec0 (nested Transformer)))
        (S1 sm'   (Rec0 (nested (Morph Contract))))
        (S1 sm''  (Rec0 (nested Function)))
        (S1 sm''' (Rec0 (nested Server)))
    where
    gServerVia (M1 (K1 trVal)) (M1 (K1 ctVal)) (M1 (K1 fnVal)) =
        M1 (K1 (serversVia trVal ctVal fnVal))

{- | Heterogeneous-@n@ counterpart to 'servers': instead of one shared @nt@
  for the whole record, each field supplies its own natural transformation
  via 'Transformer', its own contract via 'Morph' (lifting a plain
  'Contract' — see 'Okapi.Contract.morph'), and its own handler via
  'Function' — with @n@ free to differ field to field, exactly the way
  @shape@ already does. Argument order mirrors 'server': transform-like
  thing first, then contract, then function.

  Unlike 'servers', the result stays @record Server@ rather than
  @record (Server IO)@ — each field's @n@ is baked directly into the
  record's own field declarations (@getUser :: f App1 Signature1@, etc.), so
  there's no single @n@ left to normalize away, and none of
  'route'\/'scope'\/'mount'\/'handle' need one anyway.
-}
serversVia ::
    forall record.
    ( Generic (record Transformer)
    , Generic (record (Morph Contract))
    , Generic (record Function)
    , Generic (record Server)
    , GServerVia (Rep (record Transformer)) (Rep (record (Morph Contract))) (Rep (record Function)) (Rep (record Server))
    ) =>
    record Transformer ->
    record (Morph Contract) ->
    record Function ->
    record Server
serversVia transforms contracts handlers =
    to (gServerVia @(Rep (record Transformer)) @(Rep (record (Morph Contract))) @(Rep (record Function)) (from transforms) (from contracts) (from handlers))

class GHandles (enF :: Type -> Type) where
    gHandles :: enF () -> [Handle]

instance (GHandles enF) => GHandles (D1 dm enF) where
    gHandles (M1 en) = gHandles en

instance (GHandles enF) => GHandles (C1 cm enF) where
    gHandles (M1 en) = gHandles en

instance (GHandles enL, GHandles enR) => GHandles (enL :*: enR) where
    gHandles (enL :*: enR) = gHandles enL <> gHandles enR

instance GHandles (S1 sm (Rec0 (Server n shape))) where
    gHandles (M1 (K1 ep)) = [handle ep]

{- | Lets a field be a nested record of 'Server's (@moreRoutes ::
  MoreRoutes Server@) instead of one concrete 'Server' — recurses via
  'handles' itself. The pattern nests 'Server' bare (the same functor
  'servers'\/'serversVia' already produce), not "any 'Generic' type" —
  that's what keeps this from risking overlap with the instance above even
  if 'Server' itself later derives 'Generic' for unrelated reasons.
-}
instance
    ( Generic (nested Server)
    , GHandles (Rep (nested Server))
    ) =>
    GHandles (S1 sm (Rec0 (nested Server)))
    where
    gHandles (M1 (K1 epVal)) = handles epVal

{- | Same as above, but for a nested record built via 'servers' rather
  than 'serversVia' — there, the functor is @Server n@ (1-arg, @n@
  already fixed by whichever record produced it — @IO@ for 'servers'
  specifically, but left free here since nothing about collapsing to
  'Handle' cares), not bare 'Server' (2-arg). The two instances never
  overlap: @nested@'s own kind already differs between them
  (@((Type -> Type) -> Type -> Type) -> Type@ vs @(Type -> Type) -> Type@),
  before even considering that 'Server' (unapplied) and @Server n@
  (applied to a monad) are different types outright.
-}
instance
    ( Generic (nested (Server n))
    , GHandles (Rep (nested (Server n)))
    ) =>
    GHandles (S1 sm (Rec0 (nested (Server n))))
    where
    gHandles (M1 (K1 epVal)) = handles epVal

{- | Collapse every field of a record of 'Server's into the foundational
  @[Handle]@ representation — works on the output of both 'servers'
  (@record (Server IO)@) and 'serversVia' (@record Server@), since
  this only looks at each field's own 'Server' value via 'Generic', not
  at the record's own kind. Mixes freely with hand-built 'Handle's:

> handles myServers ++ [handle oneOffServer]

  From here, everything is the same ordinary list code the foundational
  mechanism already uses: 'mountAll' for the app, @foldMap 'toOpenApi'@ for docs
  derived from a fully-built record of servers (contract-only docs
  without needing handlers built at all still go through
  'Okapi.OpenApi.openApiVia' directly on the contracts record
  instead).
-}
handles :: (Generic r, GHandles (Rep r)) => r -> [Handle]
handles = gHandles . from
