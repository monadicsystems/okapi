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
import Data.Tree.Knuth.Forest qualified as Knuth
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

type Forest r = [Tree r]

-- TODO: Potentially add type parameter to constrain middleware enum type
data Tree (r :: [Type]) where
  Match ::
    forall a (r :: [Type]).
    (Show a, Web.ToHttpApiData a, Eq a, Typeable.Typeable a) =>
    a ->
    Forest r ->
    Tree r
  Param ::
    forall a (r :: [Type]).
    (Web.FromHttpApiData a, Typeable.Typeable a) =>
    Forest (r :-> a) ->
    Tree r
  Regex ::
    forall a (r :: [Type]).
    (Regex.RegexContext Regex.Regex Text.Text a, Typeable.Typeable a) =>
    Text.Text ->
    Forest (r :-> a) ->
    Tree r
  Splat ::
    forall a (r :: [Type]).
    (Web.FromHttpApiData a, Typeable.Typeable a) =>
    Forest (r :-> NonEmpty.NonEmpty a) ->
    Tree r
  Route ::
    forall a (r :: [Type]).
    (Route.Route a, Typeable.Typeable a) =>
    Forest (r :-> a) ->
    Tree r
  Query ::
    forall a (r :: [Type]).
    (Query.From a, Typeable.Typeable a) =>
    Forest (r :-> a) ->
    Tree r
  Headers ::
    forall a (r :: [Type]).
    (Headers.From a, Typeable.Typeable a) =>
    Forest (r :-> a) ->
    Tree r
  Body ::
    forall a (r :: [Type]).
    (Body.From a, Typeable.Typeable a) =>
    Forest (r :-> a) ->
    Tree r
  Apply ::
    forall t (r :: [Type]).
    (Middleware.Tag t, Eq t, Typeable.Typeable t) =>
    t ->
    Tree r ->
    Tree r
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
    ) =>
    Forest (r :-> (Response.Headers headerKeys -> resultType -> Wai.Response)) ->
    Tree r
  -- Events ::
  --   forall (r :: [Type]).
  --   (Typeable.Typeable r) =>
  --   Chan.Chan Wai.ServerEvent ->
  --   Tree r
  Method ::
    forall env (r :: [Type]).
    (Typeable.Typeable env) =>
    HTTP.StdMethod ->
    (env Natural.~> IO) ->
    Handler r env ->
    Tree r

-- TODO: Add tags to method/handlers like in reitit (Clojure)

combine ::
  forall (r :: [Type]).
  Tree r ->
  Tree r ->
  Maybe (Tree r)
combine n1 n2 = case (n1, n2) of
  (Match @a1 @r1 x child1, Match @a2 @r2 y child2) -> case (Typeable.eqT @a1 @a2) of
    Just Typeable.Refl ->
      if x == y
        then Just $ match @a1 @r1 x $ flatten $ child1 <> child2
        else Nothing
    _ -> Nothing
  (Param @a1 @r1 child1, Param @a2 @r2 child2) -> case (Typeable.eqT @a1 @a2) of
    Just Typeable.Refl -> Just $ param @a1 @r1 $ flatten $ child1 <> child2
    _ -> Nothing
  (Regex @a1 @r1 regex1 child1, Regex @a2 @r2 regex2 child2) -> case (Typeable.eqT @a1 @a2, regex1 == regex2) of
    (Just Typeable.Refl, True) -> Just $ regex @a1 @r1 regex1 $ flatten $ child1 <> child2
    (_, _) -> Nothing
  (Splat @a1 @r1 child1, Splat @a2 @r2 child2) -> case (Typeable.eqT @a1 @a2) of
    Just Typeable.Refl -> Just $ splat @a1 @r1 $ flatten @(r1 :-> NonEmpty.NonEmpty a1) $ child1 <> child2
    _ -> Nothing
  (Route @a1 @r1 child1, Route @a2 @r2 child2) -> case (Typeable.eqT @a1 @a2) of
    Just Typeable.Refl -> Just $ route @a1 @r1 $ flatten @(r1 :-> a1) $ child1 <> child2
    _ -> Nothing
  (Query @a1 @r1 child1, Query @a2 @r2 child2) -> case (Typeable.eqT @a1 @a2) of
    Just Typeable.Refl -> Just $ query @a1 @r1 $ flatten @(r1 :-> a1) $ child1 <> child2
    _ -> Nothing
  (Headers @a1 @r1 child1, Headers @a2 @r2 child2) -> case (Typeable.eqT @a1 @a2) of
    Just Typeable.Refl -> Just $ headers @a1 @r1 $ flatten @(r1 :-> a1) $ child1 <> child2
    _ -> Nothing
  (Body @a1 @r1 child1, Body @a2 @r2 child2) -> case (Typeable.eqT @a1 @a2) of
    Just Typeable.Refl -> Just $ body @a1 @r1 $ flatten @(r1 :-> a1) $ child1 <> child2
    _ -> Nothing
  (Apply @t1 @r1 tag1 node1, Apply @t2 @r2 tag2 node2) -> case (Typeable.eqT @t1 @t2) of
    Just Typeable.Refl -> case tag1 == tag2 of
      True -> case node1 `combine` node2 of
        Just newNode -> Just $ apply @t1 @r1 tag1 newNode
        Nothing -> Nothing
      False -> Nothing
    _ -> Nothing
  (Responder @s1 @hk1 @ct1 @rt1 @r1 child1, Responder @s2 @hk2 @ct2 @rt2 @r2 child2) -> case (Typeable.eqT @s1 @s2, Typeable.eqT @hk1 @hk2, Typeable.eqT @ct1 @ct2, Typeable.eqT @rt1 @rt2) of
    (Just Typeable.Refl, Just Typeable.Refl, Just Typeable.Refl, Just Typeable.Refl) -> Just $ responder @s1 @hk1 @ct1 @rt1 @r1 $ flatten @(r1 :-> (Response.Headers hk1 -> rt1 -> Wai.Response)) $ child1 <> child2
    (_, _, _, _) -> Nothing
  -- Method is not combinable
  (_, _) -> Nothing

flatten :: Forest r -> Forest r
flatten [] = []
flatten [tree] = [tree]
flatten (tree1 : tree2 : trees) = case tree1 `combine` tree2 of
  Just newTree -> flatten (newTree : trees)
  Nothing ->
    List.concat
      [ flatten (tree1 : trees)
      , flatten (tree2 : trees)
      , flatten trees
      ]

match ::
  forall a (r :: [Type]).
  (Show a, Web.ToHttpApiData a, Eq a, Typeable.Typeable a) =>
  a ->
  Forest r ->
  Tree r
match = Match @a @r

match' ::
  forall a (r :: [Type]).
  (Show a, Web.ToHttpApiData a, Eq a, Typeable.Typeable a) =>
  a ->
  Tree r ->
  Tree r
match' val = match @a @r val . pure

lit ::
  forall (r :: [Type]).
  Text.Text ->
  Forest r ->
  Tree r
lit = match @Text.Text

lit' ::
  forall (r :: [Type]).
  Text.Text ->
  Tree r ->
  Tree r
lit' = match' @Text.Text

param ::
  forall a (r :: [Type]).
  (Web.FromHttpApiData a, Typeable.Typeable a) =>
  Forest (r :-> a) ->
  Tree r
param = Param @a @r

param' ::
  forall a (r :: [Type]).
  (Web.FromHttpApiData a, Typeable.Typeable a) =>
  Tree (r :-> a) ->
  Tree r
param' = param @a @r . pure

regex ::
  forall a (r :: [Type]).
  (Regex.RegexContext Regex.Regex Text.Text a, Typeable.Typeable a) =>
  Text.Text ->
  Forest (r :-> a) ->
  Tree r
regex = Regex @a @r

regex' ::
  forall a (r :: [Type]).
  (Regex.RegexContext Regex.Regex Text.Text a, Typeable.Typeable a) =>
  Text.Text ->
  Tree (r :-> a) ->
  Tree r
regex' r = regex @a @r r . pure

splat ::
  forall a (r :: [Type]).
  (Web.FromHttpApiData a, Typeable.Typeable a) =>
  Forest (r :-> NonEmpty.NonEmpty a) ->
  Tree r
splat = Splat @a @r

splat' ::
  forall a (r :: [Type]).
  (Web.FromHttpApiData a, Typeable.Typeable a) =>
  Tree (r :-> NonEmpty.NonEmpty a) ->
  Tree r
splat' = splat @a @r . pure

route ::
  forall a (r :: [Type]).
  (Route.Route a, Typeable.Typeable a) =>
  Forest (r :-> a) ->
  Tree r
route = Route @a @r

route' ::
  forall a (r :: [Type]).
  (Route.Route a, Typeable.Typeable a) =>
  Tree (r :-> a) ->
  Tree r
route' = route @a @r . pure

query ::
  forall a (r :: [Type]).
  (Query.From a, Typeable.Typeable a) =>
  Forest (r :-> a) ->
  Tree r
query = Query @a @r

query' ::
  forall a (r :: [Type]).
  (Query.From a, Typeable.Typeable a) =>
  Tree (r :-> a) ->
  Tree r
query' = query @a @r . pure

headers ::
  forall a (r :: [Type]).
  (Headers.From a, Typeable.Typeable a) =>
  Forest (r :-> a) ->
  Tree r
headers = Headers @a @r

headers' ::
  forall a (r :: [Type]).
  (Headers.From a, Typeable.Typeable a) =>
  Tree (r :-> a) ->
  Tree r
headers' = headers @a @r . pure

body ::
  forall a (r :: [Type]).
  (Body.From a, Typeable.Typeable a) =>
  Forest (r :-> a) ->
  Tree r
body = Body @a @r

body' ::
  forall a (r :: [Type]).
  (Body.From a, Typeable.Typeable a) =>
  Tree (r :-> a) ->
  Tree r
body' = body @a @r . pure

apply ::
  forall t (r :: [Type]).
  (Middleware.Tag t, Eq t, Typeable.Typeable t) =>
  t ->
  Tree r ->
  Tree r
apply = Apply @t @r

scope ::
  forall a t (r :: [Type]).
  (Route.Route a, Typeable.Typeable a, Middleware.Tag t, Eq t, Typeable.Typeable t) =>
  t ->
  Forest (r :-> a) ->
  Tree r
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
  ) =>
  Forest (r :-> (Response.Headers headerKeys -> resultType -> Wai.Response)) ->
  Tree r
responder = Responder @status @headerKeys @contentType @resultType @r

responder' ::
  forall (status :: Nat.Nat) (headerKeys :: [Exts.Symbol]) (contentType :: Type) (resultType :: Type) (r :: [Type]).
  ( Nat.KnownNat status
  , Typeable.Typeable status
  , Response.WaiResponseHeaders headerKeys
  , Response.ContentType contentType
  , Response.ToContentType contentType resultType
  , Typeable.Typeable headerKeys
  , Typeable.Typeable contentType
  , Typeable.Typeable resultType
  ) =>
  Tree (r :-> (Response.Headers headerKeys -> resultType -> Wai.Response)) ->
  Tree r
responder' = Responder @status @headerKeys @contentType @resultType @r . pure

method ::
  forall env (r :: [Type]).
  (Typeable.Typeable env) =>
  HTTP.StdMethod ->
  (env Natural.~> IO) ->
  Handler r env ->
  Tree r
method = Method @env @r

endpoint ::
  forall a env (r :: [Type]).
  (Route.Route a, Typeable.Typeable a, Typeable.Typeable env) =>
  HTTP.StdMethod ->
  (env Natural.~> IO) ->
  Handler (r :-> a) env ->
  Tree r
endpoint stdMethod transformation handler =
  route @a [method @env @(r :-> a) stdMethod transformation handler]

any ::
  forall env (r :: [Type]).
  (Typeable.Typeable env) =>
  (env Natural.~> IO) ->
  Handler r env ->
  Forest r
any transformation handler =
  [ Method @env @r stdMethod transformation handler
  | stdMethod <- [minBound ..]
  ]

data HList (l :: [Type]) where
  HNil :: HList '[]
  HCons :: e -> HList l -> HList (e ': l)

snoc :: forall (l :: [Type]) (e :: Type). HList l -> e -> HList (l :-> e)
snoc HNil x = HCons x HNil
snoc (HCons h t) x = HCons h (snoc t x)

fillHandler :: Handler args env -> HList args -> (Wai.Request -> env Wai.Response)
fillHandler handler HNil = handler
fillHandler handler (HCons x xs) = fillHandler (handler x) xs

withDefault :: Forest '[] -> Wai.Middleware
withDefault = withDefaultLoop id HNil

withDefaultLoop :: Wai.Middleware -> HList r -> Forest r -> Wai.Middleware
withDefaultLoop middleware args root backup request respond = case root of
  [] -> backup request respond
  (node : nodes) -> case node of
    Match value subNodes ->
      case Wai.pathInfo request of
        [] -> withDefaultLoop middleware args nodes backup request respond
        (pathHead : pathTail) ->
          if pathHead == Web.toUrlPiece value
            then do
              let newRequest = request{Wai.pathInfo = pathTail}
              withDefaultLoop middleware args subNodes backup newRequest respond
            else withDefaultLoop middleware args nodes backup request respond
    Param @p subNodes ->
      case Wai.pathInfo request of
        [] -> withDefaultLoop middleware args nodes backup request respond
        (pathHead : pathTail) ->
          case Web.parseUrlPiece @p pathHead of
            Left _ -> withDefaultLoop middleware args nodes backup request respond
            Right value -> do
              let newRequest = request{Wai.pathInfo = pathTail}
              withDefaultLoop middleware (snoc args value) subNodes backup newRequest respond
    Responder @s @hk @ct @rt @r subNodes ->
      let callback = Response.makeResponder @s @hk @ct @rt
       in withDefaultLoop
            middleware
            (snoc args callback)
            subNodes
            (withDefaultLoop middleware args nodes backup)
            request
            respond
    Method stdMethod transformation handler ->
      case HTTP.parseMethod $ Wai.requestMethod request of
        Left _ -> withDefaultLoop middleware args nodes backup request respond
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
            else withDefaultLoop middleware args nodes backup request respond

-- Events source ->
--   middleware
--     ( \request' respond' -> Wai.eventSourceAppChan source request' respond'
--     )
--     request
--     respond

---- TODO: May need system for content-type negotiation???
---- The accepted content types can be the same or more
---- If Accept is less than the responseses content types, then I can't go down that tree

instance Show (Tree r) where
  show = Tree.drawTree . stringTree []

stringTree :: [String] -> Tree r -> Tree.Tree String
stringTree responderStrings node = case node of
  Match @v value subNodes -> Tree.Node ("/" <> show value <> ":" <> showType @v) (stringForest responderStrings subNodes)
  Param @p subNodes -> Tree.Node ("/:" <> showType @p) (stringForest responderStrings subNodes)
  Responder @s @hk @ct @rt @_ subNodes ->
    let
      respString =
        show (Response.natToStatus $ Nat.natVal @s Typeable.Proxy)
          <> " "
          <> (showType @hk)
          <> " "
          <> (showType @ct)
          <> " "
          <> (showType @rt)
     in
      Tree.Node "resp" $ stringForest (respString : responderStrings) subNodes
  Method @env @args stdMethod transformation handler -> Tree.Node (show stdMethod <> " " <> showType @env <> "\n" <> (List.intercalate "\n" $ map show responderStrings)) []

stringForest :: [String] -> Forest r -> Tree.Forest String
stringForest responderStrings forest = map (stringTree responderStrings) forest

-- delete :: String -> Tree.Forest String -> Tree.Forest String
-- delete _ [] = []
-- delete v (node:nodes) = case node of

printForest = putStrLn . Tree.drawForest . stringForest []

{-
deleteRespNode :: Maybe (Tree.Tree String) -> Tree.Forest String -> Tree.Forest String
deleteRespNode _ [] = []
deleteRespNode Nothing (node : siblings) = case node of -- No parent means it's the root
  Tree.Node val children -> case val == "resp" of
    True -> deleteRespTree Nothing children : deleteRespNode Nothing siblings
    False -> (deleteRespTree (Just node) children) : (deleteRespNode Nothing siblings)
deleteRespNode (Just parent@(Tree.Node pVal pChildren)) forest@(node : siblings) = case node of
  Tree.Node val children -> case val == "resp" of
    True -> (Tree.Node pVal (deleteRespNode (Just node) children)) : deleteRespNode Nothing siblings
    False -> [Tree.Node pVal (deleteRespNode Nothing pChildren)]

deleteRespTree :: Maybe (Tree.Tree String) -> Tree.Tree String -> Tree.Tree String
deleteRespTree Nothing node = case node of -- No parent means it's the root
  Tree.Node val children -> case val == "resp" of
    True -> deleteRespNode Nothing children ++ deleteRespNode Nothing siblings
    False -> d(deleteRespNode (Just node) children) <> (deleteRespNode Nothing siblings)
deleteRespTree (Just parent@(Tree.Node pVal pChildren)) node = case node of
  Tree.Node val children -> case val == "resp" of
    True -> Tree.Node pVal (deleteRespNode (Just parent) pChildren)
    False -> [Tree.Node pVal (deleteRespNode Nothing pChildren)]
-}
-- deleteRespNode :: Tree.Forest String -> Tree.Forest String
-- deleteRespNode = Knuth.toForest . knuthLoop . Knuth.fromForest

-- knuthLoop :: Knuth.KnuthForest String -> Knuth.KnuthForest String
-- knuthLoop f = case f of
--   Knuth.Nil -> Knuth.Nil
--   t -> (step2 . step1) t

-- isIn :: (Eq a) => a -> Knuth.KnuthForest a -> Bool
-- isIn x t = case t of
--   Knuth.Nil -> False
--   Knuth.Fork v _ _ -> v == x

-- step1 :: Knuth.KnuthForest String -> Knuth.KnuthForest String
-- step1 fork@(Knuth.Fork val child sibling) = case "resp" `isIn` child of
--   True ->
--     let
--       newChild = case child of
--         Knuth.Nil -> Knuth.Nil
--         Knuth.Fork _ grandChild _ -> grandChild
--      in
--       Knuth.Fork val (knuthLoop newChild) (knuthLoop sibling)
--   False -> fork

-- step2 :: Knuth.KnuthForest String -> Knuth.KnuthForest String
-- step2 fork@(Knuth.Fork val child sibling) =
--   case "resp" `isIn` sibling of
--     True ->
--       let
--         newSibling = case sibling of
--           Knuth.Nil -> Knuth.Nil
--           Knuth.Fork _ _ nextSibling -> nextSibling
--        in
--         Knuth.Fork val (knuthLoop child) (knuthLoop newSibling)
--     False -> fork

-- Events source -> Tree.Node "event source" []

showType :: forall a. (Typeable.Typeable a) => String
showType = show . Typeable.typeRep $ Typeable.Proxy @a
