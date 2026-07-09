module Okapi.Handle
    ( Handle
    , handle
    , scope
    , tryHandle
    ) where

import Network.Wai qualified as Wai
import Okapi.Mode.Forest (Forest (..), stripTags)
import Okapi.Mode.Route (Route (..))
import Okapi.Mode.Server (type (~>), tryServe)

data Handle where
    Handle ::
        (n ~> IO) ->
        [Wai.Middleware] ->
        Maybe Handle ->
        Route n shape ->
        Handle

handle :: (n ~> IO) -> Route n shape -> Handle
handle nt route = Handle nt [] Nothing route

scope :: Wai.Middleware -> Handle -> Handle
scope mw (Handle nt mws alt route) = Handle nt (mw : mws) alt route

instance Semigroup Handle where
    Handle nt mws Nothing  route <> h2 = Handle nt mws (Just h2) route
    Handle nt mws (Just a) route <> h2 = Handle nt mws (Just (a <> h2)) route

-- | Dispatch itself doesn't depend on which 'Forest' constructor @forest@
--   is — 'tryServe' handles that (and strips any 'annotate'd metadata)
--   itself — but a match is still needed here to refine @forest@'s GADT
--   type enough for 'tryServe'\'s signature to apply.
tryHandle :: Handle -> Wai.Middleware
tryHandle (Handle nt mws alt (forest := srv)) backup req respond =
    case stripTags forest of
        (_ :-> _)    -> foldr (.) id mws (tryServe nt forest srv nextApp) req respond
        (_ :-< _)    -> foldr (.) id mws (tryServe nt forest srv nextApp) req respond
        Annotate _ _ -> error "unreachable: stripTags already peeled off every Annotate layer"
  where
    nextApp :: Wai.Application
    nextApp _ignoredReq _ignoredRespond = case alt of
        Nothing   -> backup req respond
        Just altH -> tryHandle altH backup req respond
