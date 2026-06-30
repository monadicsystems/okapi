{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Okapi.HTTP.Responses where

import Data.Functor.Identity (Identity (..))
import Data.Kind (Type)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.List.NonEmpty qualified as NE
import Data.Maybe (mapMaybe)
import GHC.Generics
import Network.Wai qualified as Wai
import Okapi.Mode.Tree qualified as Tree
import Okapi.Mode.Error    qualified as Error
import Okapi.Mode.Result   qualified as Result
import Okapi.Mode.Data    qualified as Data
import Okapi.HTTP.Response qualified as Res

newtype Only
    (status  :: Type)
    (headers :: Type)
    (body    :: Type)
    (f       :: Type -> Type -> Type -> Type)
    = Only { unOnly :: f status headers body }
    deriving (Generic)

only :: f status headers body -> Only status headers body f
only = Only

newtype Responses
    (f         :: Type -> Type -> Type -> Type)
    (responses :: (Type -> Type -> Type -> Type) -> Type)
    = Responses (NonEmpty (responses f))

getResponses :: Responses f responses -> NonEmpty (responses f)
getResponses (Responses xs) = xs

type family GResponseFunc
    (f   :: Type -> Type -> Type -> Type)
    (rep :: Type -> Type)
    (res :: Type)
    :: Type where
    GResponseFunc f (D1 meta fi)                              res = GResponseFunc f fi res
    GResponseFunc f (C1 meta fi)                              res = GResponseFunc f fi res
    GResponseFunc f (S1 meta (Rec0 (f status headers body))) res = f status headers body -> res
    GResponseFunc f (fi :+: gi)                              res = GResponseFunc f fi (GResponseFunc f gi res)

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
        Generic (responses Tree.Response) =>
        (rep () -> Rep (responses Tree.Response) ()) ->
        (NonEmpty (responses Tree.Response) -> res) ->
        GResponseFunc Tree.Response rep res

instance GConstruct fi => GConstruct (D1 meta fi) where
    gConstruct inject cont = gConstruct (inject . M1) cont

instance GConstruct fi => GConstruct (C1 meta fi) where
    gConstruct inject cont = gConstruct (inject . M1) cont

instance GConstruct (S1 meta (Rec0 (Tree.Response status headers body))) where
    gConstruct inject cont codec = cont (to (inject (M1 (K1 codec))) :| [])

instance (GConstruct fil, GConstruct fir) => GConstruct (fil :+: fir) where
    gConstruct inject cont =
        gConstruct (inject . L1) $ \ls ->
        gConstruct (inject . R1) $ \rs ->
        cont (ls <> rs)

class
    ( Generic (responses Tree.Response)
    , Generic (responses Result.Response)
    , Generic (responses Data.Response)
    , Generic (responses Error.Response)
    , GConstruct (Rep (responses Tree.Response))
    , GTraverse Tree.Response Result.Response
        (Rep (responses Tree.Response)) (Rep (responses Result.Response))
    , GTraverse Result.Response   Data.Response
        (Rep (responses Result.Response))   (Rep (responses Data.Response))
    , GTraverse Result.Response   Error.Response
        (Rep (responses Result.Response))   (Rep (responses Error.Response))
    , GTraverse Tree.Response Tree.Response
        (Rep (responses Tree.Response)) (Rep (responses Tree.Response))
    , GZip Tree.Response Data.Response
        (Rep (responses Tree.Response)) (Rep (responses Data.Response))
    ) =>
    Cases (responses :: (Type -> Type -> Type -> Type) -> Type)

cases ::
    forall (responses :: (Type -> Type -> Type -> Type) -> Type).
    Cases responses =>
    GResponseFunc
        Tree.Response
        (Rep (responses Tree.Response))
        (Responses Tree.Response responses)
cases =
    gConstruct
        (id :: Rep (responses Tree.Response) () -> Rep (responses Tree.Response) ())
        (Responses @Tree.Response @responses)

parseResponses ::
    forall responses.
    Cases responses =>
    Responses Tree.Response responses ->
    Wai.Response ->
    IO (Either (Responses Error.Response responses) (responses Data.Response))
parseResponses (Responses cs) waiRes = do
    rs <- traverse parseBranch cs
    pure $ case mapMaybe toValue (NE.toList rs) of
        (v : _) -> Right v
        []      -> Left (Responses (fmap toErrors rs))
  where
    parseBranch :: responses Tree.Response -> IO (responses Result.Response)
    parseBranch = traverseResponses (\codec -> Res.parseResponseResult codec waiRes)
    toValue :: responses Result.Response -> Maybe (responses Data.Response)
    toValue = traverseResponses Res.resultToValue
    toErrors :: responses Result.Response -> responses Error.Response
    toErrors = runIdentity . traverseResponses (Identity . Res.resultToError)

printResponses ::
    forall responses.
    Cases responses =>
    Responses Tree.Response responses ->
    responses Data.Response ->
    IO Wai.Response
printResponses (Responses cs) rv =
    case [io | c <- NE.toList cs, Just io <- [zipResponses Res.printResponse c rv]] of
        (io : _) -> io
        []       -> error "printResponses: no matching response constructor"
