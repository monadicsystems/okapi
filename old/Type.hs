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
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# HLINT ignore "Use if" #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Okapi.Type where

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
import Data.Void qualified as Void
import GHC.Exts qualified as Exts
import GHC.Generics qualified as Generics
import GHC.TypeLits qualified as TypeLits
import GHC.TypeNats qualified as Nat
import Network.HTTP.Types qualified as HTTP
import Network.Wai qualified as Wai
import Network.Wai.EventSource qualified as Wai
import Network.Wai.Handler.Warp qualified as Warp
import Okapi.Body qualified as Body
import Okapi.Headers qualified as Headers

import Okapi.Query qualified as Query
import Okapi.Response qualified as Response
import Okapi.Route qualified as Route
import Text.Regex.TDFA qualified as Regex
import Web.HttpApiData qualified as Web
import Prelude hiding (or)

type Handler :: [Type] -> (Type -> Type) -> Type
type family Handler args env where
    Handler '[] env = Wai.Request -> env Wai.Response
    Handler (arg : args) env = arg -> Handler args env

type NotEqual :: TREE -> TREE -> Bool
type family NotEqual a b where
    NotEqual a a = False
    NotEqual a b = True

type TREE :: Type
data TREE where
    LEAF :: METHOD -> TREE
    GROW :: Type -> TREE -> TREE
    FORK :: TREE -> TREE -> TREE

type METHOD :: Type
data METHOD where
    GET :: METHOD
    POST :: METHOD
    PUT :: METHOD

type Root (t :: TREE) = Tree t '[]

(.>>) :: (Tree t1 p1 -> Tree t2 p2) -> (Tree t2 p2 -> Tree t3 p3) -> Tree t1 p1 -> Tree t3 p3
(.>>) mkTree1 mkTree2 = mkTree2 . mkTree1

(<.>) ::
    forall (t1 :: TypeTree) (t2 :: TypeTree) (p :: [Type]).
    (NotEqual t1 t2 ~ True) =>
    Tree t1 p ->
    Tree t2 p ->
    Tree (t1 :<|> t2) p
(<.>) = Or

test :: Root _
test = Lit @"hello" .>> Lit @"world" .>> Param @Float $ testMethods

testMethods = method1 <.> method2 <.> method3
  where
    method1 = Method @GET @IO id \_ _ (f :: Float) -> do undefined
    method2 = Method @POST @IO id \_ _ (f :: Float) -> do undefined
    method3 = Lit @"bye" $ Method @GET @IO id \_ _ _ (f :: Float) -> do undefined

-- TODO: Potentially add type parameter to constrain middleware enum type
data Tree (t :: TypeTree) (p :: [Type]) where
    -- Match ::
    --   forall a (t :: TypeTree) (f :: TypeForest) (p :: [Type]).
    --   (Show a, Web.ToHttpApiData a, Eq a, Typeable.Typeable a, t ~ TypeNode a f) =>
    --   a ->
    --   Forest f (p :-> a) ->
    --   Tree t p
    Lit ::
        forall (s :: Exts.Symbol) (t :: TypeTree) (c :: TypeTree) (p :: [Type]).
        (TypeLits.KnownSymbol s, t ~ TypeNode (Route.LIT s) c) =>
        Tree c (Route.LIT s : p) ->
        Tree t p
    Param ::
        forall a (t :: TypeTree) (c :: TypeTree) (p :: [Type]).
        (Web.FromHttpApiData a, Typeable.Typeable a, t ~ TypeNode a c) =>
        Tree c (a : p) ->
        Tree t p
    Splat ::
        forall a (t :: TypeTree) (c :: TypeTree) (p :: [Type]).
        (Web.FromHttpApiData a, Typeable.Typeable a, t ~ TypeNode (NonEmpty.NonEmpty a) c) =>
        Tree c (NonEmpty.NonEmpty a : p) ->
        Tree t p
    Route ::
        forall a (t :: TypeTree) (c :: TypeTree) (p :: [Type]).
        (Route.Route a, Typeable.Typeable a, t ~ TypeNode a c) =>
        Tree c (a : p) ->
        Tree t p
    Method ::
        forall (m :: METHOD) (env :: Type -> Type) (p :: [Type]).
        (Typeable.Typeable env) =>
        (env Natural.~> IO) ->
        Handler p env ->
        Tree (TypeLeaf m) p
    Responder ::
        forall (status :: Nat.Nat) (headerKeys :: [Exts.Symbol]) (contentType :: Type) (resultType :: Type) (t :: TypeTree) (c :: TypeTree) (p :: [Type]).
        ( Nat.KnownNat status
        , Typeable.Typeable status
        , Response.WaiResponseHeaders headerKeys
        , Response.ContentType contentType
        , Response.ToContentType contentType resultType
        , Typeable.Typeable headerKeys
        , Typeable.Typeable contentType
        , Typeable.Typeable resultType
        , t ~ TypeNode (Response.Headers headerKeys -> resultType -> Wai.Response) c
        ) =>
        Tree c ((Response.Headers headerKeys -> resultType -> Wai.Response) : p) ->
        Tree t p
    Or ::
        forall (t1 :: TypeTree) (t2 :: TypeTree) (p :: [Type]).
        (NotEqual t1 t2 ~ True) =>
        Tree t1 p ->
        Tree t2 p ->
        Tree (t1 :<|> t2) p

{-
Query ::
  forall a (t :: TypeTree) (f :: TypeForest).
  (Query.From a, Typeable.Typeable a, t ~ TypeNode a f) =>
  Forest f ->
  Tree t
Headers ::
  forall a (t :: TypeTree) (f :: TypeForest).
  (Headers.From a, Typeable.Typeable a, t ~ TypeNode a f) =>
  Forest f ->
  Tree t
Body ::
  forall a (t :: TypeTree) (f :: TypeForest).
  (Body.From a, Typeable.Typeable a, t ~ TypeNode a f) =>
  Forest f ->
  Tree t
-}

data RequestPart

class Renderable a where
    render :: a -> Text.Text

data HList (l :: [Type]) where
    HNil :: HList '[]
    HCons :: e -> HList l -> HList (e ': l)



type OR :: Bool -> Bool -> Bool
type family OR bool1 bool2 where
    OR False False = False
    OR _ _ = True

type IsIn :: METHOD -> [Type] -> TypeTree -> Bool
type family IsIn method path tree where
    IsIn method '[] (TypeLeaf method) = True
    IsIn method '[] (TypeLeaf method') = False
    IsIn method (arg : args) (TypeNode arg subTree) = IsIn method args subTree
    IsIn method args (tree :<|> tree') = IsIn method args tree `OR` IsIn method args tree'

mkURL :: (IsIn m args t ~ True) => Root t -> HList args -> [Text.Text]
mkURL tree HNil = undefined

fillHandler :: Handler args env -> HList args -> (Wai.Request -> env Wai.Response)
fillHandler handler HNil = handler
fillHandler handler (HCons x xs) = fillHandler (handler x) xs

withDefault :: Root t -> Wai.Middleware
withDefault tree backup request respond = case lookupResponse HNil tree request of
    Nothing -> backup request respond
    Just responder -> do
        response <- responder
        respond response

lookupResponse :: HList r -> Tree t r -> Wai.Request -> Maybe (IO Wai.Response)
lookupResponse args tree request = case tree of
    t1 `Or` t2 -> case lookupResponse args t1 request of
        Just response1 -> Just response1
        Nothing -> case lookupResponse args t2 request of
            Just response2 -> Just response2
            Nothing -> Nothing
    Param @p subTree ->
        case Wai.pathInfo request of
            [] -> Nothing
            (pathHead : pathTail) ->
                case Web.parseUrlPiece @p pathHead of
                    Left _ -> Nothing
                    Right value -> do
                        let newRequest = request{Wai.pathInfo = pathTail}
                        lookupResponse (HCons value args) subTree newRequest
    Responder @s @hk @ct @rt @r subTree ->
        let callback = Response.makeResponder @s @hk @ct @rt
         in lookupResponse
                (HCons callback args)
                subTree
                request
    Method @m @env @r transformation handler -> Just (transformation $ fillHandler @r @env handler args request)

-- Events source ->
--   middleware
--     ( \request' respond' -> Wai.eventSourceAppChan source request' respond'
--     )
--     request
--     respond

---- TODO: May need system for content-type negotiation???
---- The accepted content types can be the same or more
---- If Accept is less than the responseses content types, then I can't go down that tree
{-
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
-}
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
