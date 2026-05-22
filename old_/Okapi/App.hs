{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use if" #-}

module Okapi.App where

import Control.Concurrent.Chan qualified as Chan
import Control.Natural qualified as Natural
import Data.Binary.Builder qualified as Builder
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Lazy.Char8 qualified as LBSChar8
import Data.Kind
import Data.List qualified as List
import Data.List.NonEmpty qualified as NonEmpty
import Data.Text qualified as Text
import Data.Tree qualified as Tree
import Data.Type.Equality qualified as Equality
import Data.Typeable qualified as Typeable
import Data.Vault.Lazy qualified as Vault
import GHC.Exts qualified as Exts
import GHC.Generics qualified as Generics
import GHC.TypeNats qualified as Nat
import Network.HTTP.Types qualified as HTTP
import Network.Wai qualified as Wai
import Network.Wai.EventSource qualified as Wai
import Network.Wai.Handler.Warp qualified as Warp
import Okapi.Body qualified as Body
import Okapi.Headers qualified as Headers
import Okapi.Middleware qualified as Middleware
import Okapi.Query qualified as Query
import Okapi.Response qualified as Response
import Okapi.Route qualified as Route

import Text.Regex.TDFA qualified as Regex
import Web.HttpApiData qualified as Web

type (:->) :: [Type] -> Type -> [Type]
type family (:->) (a :: [Type]) (b :: Type) where
  (:->) '[] b = '[b]
  (:->) (aa : aas) b = aa : (aas :-> b)

type Handler :: [Type] -> (Type -> Type) -> Type
type family Handler args env where
  Handler '[] env = Wai.Request -> env Wai.Response
  Handler (arg : args) env = arg -> Handler args env

-- TODO: Potentially add type parameter to constrain middleware enum type
data Node (r :: [Type]) where
  Choice ::
    forall (r :: [Type]).
    -- (Typeable.Typeable r) =>
    [Node r] ->
    Node r
  Match ::
    forall a (r :: [Type]).
    (Web.ToHttpApiData a, Eq a, Typeable.Typeable a, Typeable.Typeable r) =>
    a ->
    Node r ->
    Node r
  Param ::
    forall a (r :: [Type]).
    (Web.FromHttpApiData a, Typeable.Typeable a, Typeable.Typeable r) =>
    Node (r :-> a) ->
    Node r
  Regex ::
    forall a (r :: [Type]).
    (Regex.RegexContext Regex.Regex Text.Text a, Typeable.Typeable a, Typeable.Typeable r) =>
    Text.Text ->
    Node (r :-> a) ->
    Node r
  Splat ::
    forall a (r :: [Type]).
    (Web.FromHttpApiData a, Typeable.Typeable a, Typeable.Typeable r) =>
    Node (r :-> NonEmpty.NonEmpty a) ->
    Node r
  Route ::
    forall a (r :: [Type]).
    (Route.From a, Typeable.Typeable a, Typeable.Typeable r) =>
    Node (r :-> a) ->
    Node r
  Query ::
    forall a (r :: [Type]).
    (Query.From a, Typeable.Typeable a, Typeable.Typeable r) =>
    Node (r :-> a) ->
    Node r
  Headers ::
    forall a (r :: [Type]).
    (Headers.From a, Typeable.Typeable a, Typeable.Typeable r) =>
    Node (r :-> a) ->
    Node r
  Body ::
    forall a (r :: [Type]).
    (Body.From a, Typeable.Typeable a, Typeable.Typeable r) =>
    Node (r :-> a) ->
    Node r
  Apply ::
    forall t (r :: [Type]).
    (Middleware.Tag t, Eq t, Typeable.Typeable t, Typeable.Typeable r) =>
    t ->
    Node r ->
    Node r
  Responder ::
    forall (status :: Nat.Nat) (headerKeys :: [Exts.Symbol]) (contentType :: Type) (resultType :: Type) (r :: [Type]).
    ( Nat.KnownNat status
    , Typeable.Typeable status
    , Response.WaiResponseHeaders headerKeys
    , Response.ContentType contentType
    , Response.ToContentType contentType resultType
    , Typeable.Typeable headerKeys
    , Typeable.Typeable contentType
    , Typeable.Typeable resultType
    , Typeable.Typeable r
    ) =>
    Node (r :-> (Response.Headers headerKeys -> resultType -> Wai.Response)) ->
    Node r
  Events ::
    forall (r :: [Type]).
    (Typeable.Typeable r) =>
    Chan.Chan Wai.ServerEvent ->
    Node r
  Method ::
    forall env (r :: [Type]).
    (Typeable.Typeable r) =>
    HTTP.StdMethod ->
    (env Natural.~> IO) ->
    Handler r env ->
    Node r

-- TODO: Add tags to method/handlers like in reitit (Clojure)

combine ::
  forall (r :: [Type]).
  (Typeable.Typeable r) =>
  Node r ->
  Node r ->
  Maybe (Node r)
combine n1 n2 = case (n1, n2) of
  (Choice @r1 children1, Choice @r2 children2) -> case (Typeable.eqT @r1 @r2) of
    Just Typeable.Refl -> Just $ choice @r1 (children1 <> children2)
    _ -> Nothing
  (Match @a1 @r1 x child1, Match @a2 @r2 y child2) -> case (Typeable.eqT @a1 @a2, Typeable.eqT @r1 @r2) of
    (Just Typeable.Refl, Just Typeable.Refl) -> if x == y then Just $ match @a1 @r1 x $ choice @r1 [child1, child2] else Nothing
    (_, _) -> Nothing
  (Param @a1 @r1 child1, Param @a2 @r2 child2) -> case (Typeable.eqT @a1 @a2, Typeable.eqT @r1 @r2) of
    (Just Typeable.Refl, Just Typeable.Refl) -> Just $ param @a1 @r1 $ choice @(r1 :-> a1) [child1, child2]
    (_, _) -> Nothing
  (Regex @a1 @r1 regex1 child1, Regex @a2 @r2 regex2 child2) -> case (Typeable.eqT @a1 @a2, Typeable.eqT @r1 @r2, regex1 == regex2) of
    (Just Typeable.Refl, Just Typeable.Refl, True) -> Just $ regex @a1 @r1 regex1 $ choice @(r1 :-> a1) [child1, child2]
    (_, _, _) -> Nothing
  (Splat @a1 @r1 child1, Splat @a2 @r2 child2) -> case (Typeable.eqT @a1 @a2, Typeable.eqT @r1 @r2) of
    (Just Typeable.Refl, Just Typeable.Refl) -> Just $ splat @a1 @r1 $ choice @(r1 :-> NonEmpty.NonEmpty a1) [child1, child2]
    (_, _) -> Nothing
  (Route @a1 @r1 child1, Route @a2 @r2 child2) -> case (Typeable.eqT @a1 @a2, Typeable.eqT @r1 @r2) of
    (Just Typeable.Refl, Just Typeable.Refl) -> Just $ route @a1 @r1 $ choice @(r1 :-> a1) [child1, child2]
    (_, _) -> Nothing
  (Query @a1 @r1 child1, Query @a2 @r2 child2) -> case (Typeable.eqT @a1 @a2, Typeable.eqT @r1 @r2) of
    (Just Typeable.Refl, Just Typeable.Refl) -> Just $ query @a1 @r1 $ choice @(r1 :-> a1) [child1, child2]
    (_, _) -> Nothing
  (Headers @a1 @r1 child1, Headers @a2 @r2 child2) -> case (Typeable.eqT @a1 @a2, Typeable.eqT @r1 @r2) of
    (Just Typeable.Refl, Just Typeable.Refl) -> Just $ headers @a1 @r1 $ choice @(r1 :-> a1) [child1, child2]
    (_, _) -> Nothing
  (Body @a1 @r1 child1, Body @a2 @r2 child2) -> case (Typeable.eqT @a1 @a2, Typeable.eqT @r1 @r2) of
    (Just Typeable.Refl, Just Typeable.Refl) -> Just $ body @a1 @r1 $ choice @(r1 :-> a1) [child1, child2]
    (_, _) -> Nothing
  (Apply @t1 @r1 tag1 node1, Apply @t2 @r2 tag2 node2) -> case (Typeable.eqT @t1 @t2, Typeable.eqT @r1 @r2) of
    (Just Typeable.Refl, Just Typeable.Refl) -> case tag1 == tag2 of
      True -> Just $ apply @t1 @r1 tag1 $ choice @r1 [node1, node2]
      False -> Nothing
    (_, _) -> Nothing
  (Responder @s1 @hk1 @ct1 @rt1 @r1 child1, Responder @s2 @hk2 @ct2 @rt2 @r2 child2) -> case (Typeable.eqT @s1 @s2, Typeable.eqT @hk1 @hk2, Typeable.eqT @ct1 @ct2, Typeable.eqT @rt1 @rt2, Typeable.eqT @r1 @r2) of
    (Just Typeable.Refl, Just Typeable.Refl, Just Typeable.Refl, Just Typeable.Refl, Just Typeable.Refl) -> Just $ responder @s1 @hk1 @ct1 @rt1 @r1 $ choice @(r1 :-> (Response.Headers hk1 -> rt1 -> Wai.Response)) [child1, child2]
    (_, _, _, _, _) -> Nothing
  (Choice @r1 children, a2') -> Just $ choice @r1 (children <> [a2'])
  (a1', Choice @r2 children) -> Just $ choice @r2 (a1' : children)
  -- Method is not comparable
  (_, _) -> Nothing

flatten :: (Typeable.Typeable r) => Node r -> Node r
flatten (Choice [node]) = node
flatten (Choice (node1 : node2 : nodes)) = case node1 `combine` node2 of
  Just newNode -> flatten $ choice (newNode : nodes)
  Nothing ->
    choice
      [ flatten $ choice (node1 : nodes)
      , flatten $ choice (node2 : nodes)
      , flatten $ choice nodes
      ]
flatten node = node

choice ::
  forall (r :: [Type]).
  -- (Typeable.Typeable r) =>
  [Node r] ->
  Node r
choice = Choice @r

match ::
  forall a (r :: [Type]).
  (Web.ToHttpApiData a, Eq a, Typeable.Typeable a, Typeable.Typeable r) =>
  a ->
  Node r ->
  Node r
match = Match @a @r

lit ::
  forall (r :: [Type]).
  (Typeable.Typeable r) =>
  Text.Text ->
  Node r ->
  Node r
lit = match @Text.Text

param ::
  forall a (r :: [Type]).
  (Web.FromHttpApiData a, Typeable.Typeable a, Typeable.Typeable r) =>
  Node (r :-> a) ->
  Node r
param = Param @a @r

regex ::
  forall a (r :: [Type]).
  (Regex.RegexContext Regex.Regex Text.Text a, Typeable.Typeable a, Typeable.Typeable r) =>
  Text.Text ->
  Node (r :-> a) ->
  Node r
regex = Regex @a @r

splat ::
  forall a (r :: [Type]).
  (Web.FromHttpApiData a, Typeable.Typeable a, Typeable.Typeable r) =>
  Node (r :-> NonEmpty.NonEmpty a) ->
  Node r
splat = Splat @a @r

route ::
  forall a (r :: [Type]).
  (Route.From a, Typeable.Typeable a, Typeable.Typeable r) =>
  Node (r :-> a) ->
  Node r
route = Route @a @r

query ::
  forall a (r :: [Type]).
  (Query.From a, Typeable.Typeable a, Typeable.Typeable r) =>
  Node (r :-> a) ->
  Node r
query = Query @a @r

headers ::
  forall a (r :: [Type]).
  (Headers.From a, Typeable.Typeable a, Typeable.Typeable r) =>
  Node (r :-> a) ->
  Node r
headers = Headers @a @r

body ::
  forall a (r :: [Type]).
  (Body.From a, Typeable.Typeable a, Typeable.Typeable r) =>
  Node (r :-> a) ->
  Node r
body = Body @a @r

apply ::
  forall t (r :: [Type]).
  (Middleware.Tag t, Eq t, Typeable.Typeable t, Typeable.Typeable r) =>
  t ->
  Node r ->
  Node r
apply = Apply @t @r

scope ::
  forall a t (r :: [Type]).
  (Route.From a, Typeable.Typeable a, Middleware.Tag t, Eq t, Typeable.Typeable t, Typeable.Typeable r) =>
  t ->
  Node (r :-> a) ->
  Node r
scope tag children = apply @t @r tag $ route @a @r children

responder ::
  forall (status :: Nat.Nat) (headerKeys :: [Exts.Symbol]) (contentType :: Type) (resultType :: Type) (r :: [Type]).
  ( Nat.KnownNat status
  , Typeable.Typeable status
  , Response.WaiResponseHeaders headerKeys
  , Response.ContentType contentType
  , Response.ToContentType contentType resultType
  , Typeable.Typeable headerKeys
  , Typeable.Typeable contentType
  , Typeable.Typeable resultType
  , Typeable.Typeable r
  ) =>
  Node (r :-> (Response.Headers headerKeys -> resultType -> Wai.Response)) ->
  Node r
responder = Responder @status @headerKeys @contentType @resultType @r

method ::
  forall env (r :: [Type]).
  (Typeable.Typeable r) =>
  HTTP.StdMethod ->
  (env Natural.~> IO) ->
  Handler r env ->
  Node r
method = Method @env @r

endpoint ::
  forall a env (r :: [Type]).
  (Route.From a, Typeable.Typeable a, Typeable.Typeable r, Typeable.Typeable (r :-> a)) =>
  HTTP.StdMethod ->
  (env Natural.~> IO) ->
  Handler (r :-> a) env ->
  Node r
endpoint stdMethod transformation handler =
  route @a $ method @env @(r :-> a) stdMethod transformation handler

any ::
  forall env (r :: [Type]).
  (Typeable.Typeable r) =>
  (env Natural.~> IO) ->
  Handler r env ->
  Node r
any transformation handler =
  choice
    [ Method @env @r stdMethod transformation handler
    | stdMethod <- [minBound ..]
    ]

events ::
  forall (r :: [Type]).
  (Typeable.Typeable r) =>
  Chan.Chan Wai.ServerEvent ->
  Node r
events = Events @r

data HList (l :: [Type]) where
  HNil :: HList '[]
  HCons :: e -> HList l -> HList (e ': l)

snoc :: forall (l :: [Type]) (e :: Type). HList l -> e -> HList (l :-> e)
snoc HNil x = HCons x HNil
snoc (HCons h t) x = HCons h (snoc t x)

fillHandler :: Handler args env -> HList args -> (Wai.Request -> env Wai.Response)
fillHandler handler HNil = handler
fillHandler handler (HCons x xs) = fillHandler (handler x) xs

withDefault :: Node '[] -> Wai.Middleware
withDefault = withDefaultLoop id HNil

withDefaultLoop :: Wai.Middleware -> HList args -> Node args -> Wai.Middleware
withDefaultLoop middleware args root backup request respond = case root of
  Choice [] -> backup request respond
  Choice (node : nodes) -> case node of
    Choice subNodes -> withDefaultLoop middleware args (choice (subNodes <> nodes)) backup request respond
    Match value subNode ->
      case Wai.pathInfo request of
        [] -> withDefaultLoop middleware args (choice nodes) backup request respond
        (pathHead : pathTail) ->
          if pathHead == Web.toUrlPiece value
            then do
              let newRequest = request{Wai.pathInfo = pathTail}
              withDefaultLoop middleware args subNode backup newRequest respond
            else withDefaultLoop middleware args (choice nodes) backup request respond
    Param @p subNode ->
      case Wai.pathInfo request of
        [] -> withDefaultLoop middleware args (choice nodes) backup request respond
        (pathHead : pathTail) ->
          case Web.parseUrlPiece @p pathHead of
            Left _ -> withDefaultLoop middleware args (choice nodes) backup request respond
            Right value -> do
              let newRequest = request{Wai.pathInfo = pathTail}
              withDefaultLoop middleware (snoc args value) subNode backup newRequest respond
    Responder @s @hk @ct @rt @r subNode ->
      let callback = Response.makeResponder @s @hk @ct @rt
       in withDefaultLoop
            middleware
            (snoc args callback)
            (choice [subNode])
            (withDefaultLoop middleware args (choice nodes) backup)
            request
            respond
    Method stdMethod transformation handler ->
      case HTTP.parseMethod $ Wai.requestMethod request of
        Left _ -> withDefaultLoop middleware args (choice nodes) backup request respond
        Right stdMethod' ->
          if stdMethod == stdMethod' && List.null (Wai.pathInfo request)
            then
              middleware
                ( \request' respond' -> do
                    response <- transformation $ fillHandler handler args request'
                    respond' response
                )
                request
                respond
            else withDefaultLoop middleware args (choice nodes) backup request respond
    Events source ->
      middleware
        ( \request' respond' -> Wai.eventSourceAppChan source request' respond'
        )
        request
        respond
  root' -> withDefaultLoop middleware args (choice [root']) backup request respond

---- TODO: May need system for content-type negotiation???
---- The accepted content types can be the same or more
---- If Accept is less than the responseses content types, then I can't go down that tree

{-
stringify :: Tree -> IO (Node.Tree String)
stringify [] = return []
stringify (tree:remForest) = case tree of
  Match value subForest -> do
    stringSubForest <- stringify subForest
    stringRemForest <- stringify remForest
    let string = "/" <> (Text.unpack $ Web.toUrlPiece value)
    return ((Tree.Node string stringSubForest) : stringRemForest)
  Param @t growSubForest -> do
    secret <- Secret.new @t
    stringSubForest <- stringify $ growSubForest secret
    stringRemForest <- stringify remForest
    let string = "/:" <> showType @t
    return ((Tree.Node string stringSubForest) : stringRemForest)
  Regex @t regex growSubForest -> do
    secret <- Secret.new @t
    stringSubForest <- stringify $ growSubForest secret
    stringRemForest <- stringify remForest
    let string = "/<" <> Text.unpack regex <> ">"
    return ((Tree.Node string stringSubForest) : stringRemForest)
  Splat @t growSubForest -> do
    secret <- Secret.new @(NonEmpty.NonEmpty ty)
    forest <- mapM $ produce secret
    return $ Tree.Node ("/*" <> showType @ty) forest
  (Route @ty route produce) = do
    secret <- Secret.new @ty
    forest <- mapM $ produce secret
    return $ Tree.Node (Text.unpack (Route.rep route)) forest
  (Method m _ _) = do
    return $ Tree.Node (show m) []
  (Apply _ api) = do
    (Tree.Node root subTrees) <- api
    return $ Tree.Node ("(" <> root <> ")") subTrees
-}

{-
forest :: Tree -> IO (Tree.Node String)
forest [] = return $ Tree.Node ":root:" []
forest apis = do
  forest' <- mapM tree apis
  return $ Tree.Node "\ESC[31m:root:\ESC[0m" forest'
  where
    tree :: Node -> IO (Tree.Node String)
    tree (Match value apis) = do
      forest <- mapM tree apis
      return $ Tree.Node ("/" <> (Text.unpack $ Web.toUrlPiece value)) forest
    tree (Param @ty produce) = do
      secret <- Secret.new @ty
      forest <- mapM tree $ produce secret
      return $ Tree.Node ("/:" <> showType @ty) forest
    tree (Regex @ty regex produce) = do
      secret <- Secret.new @ty
      forest <- mapM tree $ produce secret
      return $ Tree.Node ("/r<" <> Text.unpack regex <> ">") forest
    tree (Splat @ty produce) = do
      secret <- Secret.new @(NonEmpty.NonEmpty ty)
      forest <- mapM tree $ produce secret
      return $ Tree.Node ("/*" <> showType @ty) forest
    tree (Route @ty route produce) = do
      secret <- Secret.new @ty
      forest <- mapM tree $ produce secret
      return $ Tree.Node (Text.unpack (Route.rep route)) forest
    tree (Method m _ _) = do
      return $ Tree.Node (show m) []
    tree (Apply _ api) = do
      (Tree.Node root subTrees) <- tree api
      return $ Tree.Node ("(" <> root <> ")") subTrees

showType :: forall a. (Typeable.Typeable a) => String
showType = show . Typeable.typeRep $ Typeable.Proxy @a

get_ = method HTTP.GET

getIO_ = method HTTP.GET id
-}
