{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module Okapi.HTTP.Responses
    ( Responses
    , getResponses
    , Cases
    , cases
    , parseResponses
    , printResponses
    , traverseResponses
    ) where

import Data.Functor.Identity (Identity (..))
import Data.Kind (Type)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.List.NonEmpty qualified as NE
import Data.Maybe (mapMaybe)
import GHC.Generics
import GHC.TypeLits (ErrorMessage (..), TypeError)
import Network.Wai qualified as Wai
import Okapi.HTTP.Response qualified as Res
import Okapi.Data.Response qualified as Data
import Okapi.Result.Response qualified as Result
import Okapi.Failure.Response qualified as Error

-- $setup
-- >>> :set -XTypeApplications
-- >>> import Network.HTTP.Types qualified as Types
-- >>> import Network.Wai qualified as Wai
-- >>> import Data.ByteString.Lazy qualified as LBS
-- >>> import GHC.Generics (Generic)
-- >>> import Okapi.HTTP.Response qualified as Res
-- >>> import Okapi.HTTP.Response.Status qualified as Status
-- >>> import Okapi.Data.Response qualified as Data
-- >>> data ExampleResponses f = ExOk (f (Status.KnownStatus 200) Types.ResponseHeaders (IO LBS.ByteString)) | ExNotFound (f (Status.KnownStatus 404) Types.ResponseHeaders (IO LBS.ByteString)) deriving Generic
-- >>> instance Cases ExampleResponses

newtype Responses
    (f         :: Type -> Type -> Type -> Type)
    (responses :: (Type -> Type -> Type -> Type) -> Type)
    = Responses (NonEmpty (responses f))

getResponses :: Responses f responses -> NonEmpty (responses f)
getResponses (Responses xs) = xs

type family GArgs
    (f   :: Type -> Type -> Type -> Type)
    (rep :: Type -> Type)
    (res :: Type)
    :: Type where
    GArgs f (D1 meta fi)                              res = GArgs f fi res
    GArgs f (C1 meta fi)                              res = GArgs f fi res
    GArgs f (S1 meta (Rec0 (f status headers body))) res = f status headers body -> res
    GArgs f (fi :+: gi)                              res = GArgs f fi (GArgs f gi res)

class GTraverse
    (f  :: Type -> Type -> Type -> Type)
    (g  :: Type -> Type -> Type -> Type)
    (fi :: Type -> Type)
    (fg :: Type -> Type) where
    gtraverse ::
        Functor t =>
        (forall status headers body. f status headers body -> t (g status headers body)) ->
        fi () -> t (fg ())

instance GTraverse f g fi fg => GTraverse f g (D1 meta fi) (D1 meta fg) where
    gtraverse k (M1 x) = M1 <$> gtraverse k x

instance GTraverse f g fi fg => GTraverse f g (C1 meta fi) (C1 meta fg) where
    gtraverse k (M1 x) = M1 <$> gtraverse k x

instance GTraverse f g
    (S1 meta (Rec0 (f status headers body)))
    (S1 meta (Rec0 (g status headers body))) where
    gtraverse k (M1 (K1 resp)) = M1 . K1 <$> k resp

instance (GTraverse f g fil fgl, GTraverse f g fir fgr)
    => GTraverse f g (fil :+: fir) (fgl :+: fgr) where
    gtraverse k (L1 x) = L1 <$> gtraverse k x
    gtraverse k (R1 x) = R1 <$> gtraverse k x

instance {-# OVERLAPPABLE #-}
    TypeError
        ( 'Text "Cannot traverse this `responses` type — its shape isn't supported."
        ':$$: 'Text "Every constructor must hold exactly one field of type `f status headers body`."
        ':$$: 'Text "Example: data MyResponses f = Ok (f 200 () Body) | NotFound (f 404 () ())"
        )
    => GTraverse f g fi fg where
    gtraverse _ = error "unreachable: resolved via TypeError instance"

traverseResponses ::
    forall f g responses t.
    ( Generic (responses f)
    , Generic (responses g)
    , GTraverse f g (Rep (responses f)) (Rep (responses g))
    , Functor t
    ) =>
    (forall status headers body. f status headers body -> t (g status headers body)) ->
    responses f -> t (responses g)
traverseResponses k = fmap to . gtraverse k . from

class GZip
    (f  :: Type -> Type -> Type -> Type)
    (g  :: Type -> Type -> Type -> Type)
    (fi :: Type -> Type)
    (fg :: Type -> Type) where
    gzip ::
        (forall status headers body. f status headers body -> g status headers body -> c) ->
        fi () -> fg () -> Maybe c

instance GZip f g fi fg => GZip f g (D1 meta fi) (D1 meta fg) where
    gzip k (M1 a) (M1 b) = gzip k a b

instance GZip f g fi fg => GZip f g (C1 meta fi) (C1 meta fg) where
    gzip k (M1 a) (M1 b) = gzip k a b

instance GZip f g
    (S1 meta (Rec0 (f status headers body)))
    (S1 meta (Rec0 (g status headers body))) where
    gzip k (M1 (K1 a)) (M1 (K1 b)) = Just (k a b)

instance (GZip f g fil fgl, GZip f g fir fgr)
    => GZip f g (fil :+: fir) (fgl :+: fgr) where
    gzip k (L1 a) (L1 b) = gzip k a b
    gzip k (R1 a) (R1 b) = gzip k a b
    gzip _ _      _      = Nothing

instance {-# OVERLAPPABLE #-}
    TypeError
        ( 'Text "Cannot zip this `responses` type — its shape isn't supported."
        ':$$: 'Text "Every constructor must hold exactly one field of type `f status headers body`."
        ':$$: 'Text "Example: data MyResponses f = Ok (f 200 () Body) | NotFound (f 404 () ())"
        )
    => GZip f g fi fg where
    gzip _ = error "unreachable: resolved via TypeError instance"

zipResponses ::
    forall f g responses c.
    ( Generic (responses f)
    , Generic (responses g)
    , GZip f g (Rep (responses f)) (Rep (responses g))
    ) =>
    (forall status headers body. f status headers body -> g status headers body -> c) ->
    responses f -> responses g -> Maybe c
zipResponses k a b = gzip k (from a) (from b)

class GConstruct (rep :: Type -> Type) where
    gConstruct ::
        forall (responses :: (Type -> Type -> Type -> Type) -> Type) res.
        Generic (responses Res.Response) =>
        (rep () -> Rep (responses Res.Response) ()) ->
        (NonEmpty (responses Res.Response) -> res) ->
        GArgs Res.Response rep res

instance GConstruct fi => GConstruct (D1 meta fi) where
    gConstruct inject cont = gConstruct (inject . M1) cont

instance GConstruct fi => GConstruct (C1 meta fi) where
    gConstruct inject cont = gConstruct (inject . M1) cont

instance GConstruct (S1 meta (Rec0 (Res.Response status headers body))) where
    gConstruct inject cont codec = cont (to (inject (M1 (K1 codec))) :| [])

instance (GConstruct fil, GConstruct fir) => GConstruct (fil :+: fir) where
    gConstruct inject cont =
        gConstruct (inject . L1) $ \ls ->
        gConstruct (inject . R1) $ \rs ->
        cont (ls <> rs)

instance {-# OVERLAPPABLE #-}
    TypeError
        ( 'Text "Cannot build `cases` for this `responses` type — its shape isn't supported."
        ':$$: 'Text "Every constructor must hold exactly one field of type `f status headers body`."
        ':$$: 'Text "Example: data MyResponses f = Ok (f 200 () Body) | NotFound (f 404 () ())"
        )
    => GConstruct rep where
    gConstruct = error "unreachable: resolved via TypeError instance"

class
    ( Generic (responses Res.Response)
    , Generic (responses Result.Response)
    , Generic (responses Data.Response)
    , Generic (responses Error.Response)
    , GConstruct (Rep (responses Res.Response))
    , GTraverse Res.Response Result.Response
        (Rep (responses Res.Response)) (Rep (responses Result.Response))
    , GTraverse Result.Response   Data.Response
        (Rep (responses Result.Response))   (Rep (responses Data.Response))
    , GTraverse Result.Response   Error.Response
        (Rep (responses Result.Response))   (Rep (responses Error.Response))
    , GTraverse Res.Response Res.Response
        (Rep (responses Res.Response)) (Rep (responses Res.Response))
    , GZip Res.Response Data.Response
        (Rep (responses Res.Response)) (Rep (responses Data.Response))
    ) =>
    Cases (responses :: (Type -> Type -> Type -> Type) -> Type)

cases ::
    forall (responses :: (Type -> Type -> Type -> Type) -> Type).
    Cases responses =>
    GArgs
        Res.Response
        (Rep (responses Res.Response))
        (Responses Res.Response responses)
cases =
    gConstruct
        (id :: Rep (responses Res.Response) () -> Rep (responses Res.Response) ())
        (Responses @Res.Response @responses)

parseResponses ::
    forall responses.
    Cases responses =>
    Responses Res.Response responses ->
    Wai.Response ->
    IO (Either (Responses Error.Response responses) (responses Data.Response))
parseResponses (Responses cs) waiRes = do
    rs <- traverse parseBranch cs
    pure $ case mapMaybe toValue (NE.toList rs) of
        (v : _) -> Right v
        []      -> Left (Responses (fmap toErrors rs))
  where
    parseBranch :: responses Res.Response -> IO (responses Result.Response)
    parseBranch = traverseResponses (\codec -> Res.parser' codec waiRes)
    toValue :: responses Result.Response -> Maybe (responses Data.Response)
    toValue = traverseResponses Res.resultToValue
    toErrors :: responses Result.Response -> responses Error.Response
    toErrors = runIdentity . traverseResponses (Identity . Res.resultToError)

-- | 'Responses'' constructor isn't exported, so 'cases' is the only way to
--   build one — and 'GConstruct'\'s @(':+:')@ instance always combines
--   @ls <> rs@, never drops a branch, so the result is guaranteed to carry
--   exactly one codec per constructor of @responses@. Any @responses
--   Data.Response@ value handed to 'printResponses' was necessarily built
--   through one of those same (finitely many) constructors, and 'zipResponses'
--   only ever returns 'Just' when the codec and the value share a
--   constructor — so a match always exists and the @error@ branch below is
--   unreachable for any 'Responses' actually produced by 'cases'. Confirmed
--   here across every constructor of a small example type sharing one
--   'cases'-built value, not just one:
--
-- >>> let cs = cases @ExampleResponses Res.ok Res.notFound
-- >>> r1 <- printResponses cs (ExOk (Data.Response { status = 200, headers = [], body = pure "hi" }))
-- >>> Types.statusCode (Wai.responseStatus r1)
-- 200
-- >>> r2 <- printResponses cs (ExNotFound (Data.Response { status = 404, headers = [], body = pure "nope" }))
-- >>> Types.statusCode (Wai.responseStatus r2)
-- 404
printResponses ::
    forall responses.
    Cases responses =>
    Responses Res.Response responses ->
    responses Data.Response ->
    IO Wai.Response
printResponses (Responses cs) rv =
    case [io | c <- NE.toList cs, Just io <- [zipResponses Res.printer c rv]] of
        (io : _) -> io
        []       -> error "printResponses: no matching response constructor"
