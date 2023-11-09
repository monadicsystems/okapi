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

import Control.Natural qualified as Natural
import Data.Binary.Builder qualified as Builder
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Lazy.Char8 qualified as LBSChar8
import Data.Functor.Base qualified as Base
import Data.Functor.Foldable qualified as Foldable
import Data.Functor.Identity qualified as Identity
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
import GHC.Natural qualified as Natural
import Network.HTTP.Types qualified as HTTP
import Network.Wai qualified as Wai
import Network.Wai.Handler.Warp qualified as Warp
import Okapi.Body qualified as Body
import Okapi.Headers qualified as Headers
import Okapi.Middleware qualified as Middleware
import Okapi.Query qualified as Query
import Okapi.Response qualified as Response
import Okapi.Route qualified as Route
import Okapi.Secret qualified as Secret
import Text.Regex.TDFA qualified as Regex
import Web.HttpApiData qualified as Web

type (:>) :: [Type] -> Type -> [Type]
type family (:>) (a :: [Type]) (b :: Type) where
  (:>) '[] b = '[b]
  (:>) (aa : aas) b = aa : (aas :> b)

type Handler :: [Type] -> (Type -> Type) -> Type
type family Handler args env where
  Handler '[] env = Wai.Request -> env Wai.Response
  Handler (arg : args) env = arg -> Handler args env

data Atom (r :: [Type]) where
  Match :: forall a (r :: [Type]). (Web.ToHttpApiData a, Eq a, Typeable.Typeable a, Typeable.Typeable r) => a -> [Atom r] -> Atom r
  Param :: forall a (r :: [Type]). (Web.FromHttpApiData a, Typeable.Typeable a, Typeable.Typeable r) => [Atom (r :> a)] -> Atom r
  Regex :: forall a (r :: [Type]). (Regex.RegexContext Regex.Regex Text.Text a, Typeable.Typeable a, Typeable.Typeable r) => Text.Text -> [Atom (r :> a)] -> Atom r
  Splat :: forall a (r :: [Type]). (Web.FromHttpApiData a, Typeable.Typeable a, Typeable.Typeable r) => [Atom (r :> NonEmpty.NonEmpty a)] -> Atom r
  Route :: forall a (r :: [Type]). (Route.From a, Typeable.Typeable a, Typeable.Typeable r) => [Atom (r :> a)] -> Atom r
  Query :: forall a (r :: [Type]). (Query.From a, Typeable.Typeable a, Typeable.Typeable r) => [Atom (r :> a)] -> Atom r
  Headers :: forall a (r :: [Type]). (Headers.From a, Typeable.Typeable a, Typeable.Typeable r) => [Atom (r :> a)] -> Atom r
  Body :: forall a (r :: [Type]). (Body.From a, Typeable.Typeable a, Typeable.Typeable r) => [Atom (r :> a)] -> Atom r
  Apply :: forall t (r :: [Type]). (Middleware.Tag t, Eq t, Typeable.Typeable t, Typeable.Typeable r) => t -> Atom r -> Atom r
  Respond :: forall a (r :: [Type]). (Response.To a, Typeable.Typeable a, Typeable.Typeable r) => [Atom (r :> a)] -> Atom r
  Method :: forall env (r :: [Type]). (Typeable.Typeable r) => HTTP.StdMethod -> (env Natural.~> IO) -> Handler r env -> Atom r

{-
instance Eq (Atom r) where
  a1 == a2 = case (a1, a2) of
    (Match @a1 @r1 x _, Match @a2 @r2 y _) -> case (Typeable.eqT @a1 @a2, Typeable.eqT @r1 @r2) of
      (Just Typeable.Refl, Just Typeable.Refl) -> x == y
      (_, _) -> False
    (Param @a1 @r1 _, Param @a2 @r2 _) -> case (Typeable.eqT @a1 @a2, Typeable.eqT @r1 @r2) of
      (Just Typeable.Refl, Just Typeable.Refl) -> True
      (_, _) -> False
    (Regex @a1 @r1 regex1 _, Regex @a2 @r2 regex2 _) -> case (Typeable.eqT @a1 @a2, Typeable.eqT @r1 @r2) of
      (Just Typeable.Refl, Just Typeable.Refl) -> regex1 == regex2
      (_, _) -> False
    (Splat @a1 @r1 _, Splat @a2 @r2 _) -> case (Typeable.eqT @a1 @a2, Typeable.eqT @r1 @r2) of
      (Just Typeable.Refl, Just Typeable.Refl) -> True
      (_, _) -> False
    (Route @a1 @r1 _, Route @a2 @r2 _) -> case (Typeable.eqT @a1 @a2, Typeable.eqT @r1 @r2) of
      (Just Typeable.Refl, Just Typeable.Refl) -> True
      (_, _) -> False
    (Query @a1 @r1 _, Query @a2 @r2 _) -> case (Typeable.eqT @a1 @a2, Typeable.eqT @r1 @r2) of
      (Just Typeable.Refl, Just Typeable.Refl) -> True
      (_, _) -> False
    (Headers @a1 @r1 _, Headers @a2 @r2 _) -> case (Typeable.eqT @a1 @a2, Typeable.eqT @r1 @r2) of
      (Just Typeable.Refl, Just Typeable.Refl) -> True
      (_, _) -> False
    (Body @a1 @r1 _, Body @a2 @r2 _) -> case (Typeable.eqT @a1 @a2, Typeable.eqT @r1 @r2) of
      (Just Typeable.Refl, Just Typeable.Refl) -> True
      (_, _) -> False
    (Apply @t1 @r1 tag1 atom1, Apply @t2 @r2 tag2 atom2) -> case (Typeable.eqT @t1 @t2, Typeable.eqT @r1 @r2) of
      (Just Typeable.Refl, Just Typeable.Refl) -> tag1 == tag2 && atom1 == atom2
      (_, _) -> False
    (Respond @a1 @r1 _, Respond @a2 @r2 _) -> case (Typeable.eqT @a1 @a2, Typeable.eqT @r1 @r2) of
      (Just Typeable.Refl, Just Typeable.Refl) -> True
      (_, _) -> False
    -- Method is not comparable
    (_, _) -> False
-}

smush :: Atom r -> Atom r -> Maybe (Atom r)
smush a1 a2 = case (a1, a2) of
  (Match @a1 @r1 x children1, Match @a2 @r2 y children2) -> case (Typeable.eqT @a1 @a2, Typeable.eqT @r1 @r2) of
    (Just Typeable.Refl, Just Typeable.Refl) -> Just $ match @a1 @r1 x $ children1 <> children2
    (_, _) -> Nothing
  (Param @a1 @r1 children1, Param @a2 @r2 children2) -> case (Typeable.eqT @a1 @a2, Typeable.eqT @r1 @r2) of
    (Just Typeable.Refl, Just Typeable.Refl) -> Just $ param @a1 @r1 $ children1 <> children2
    (_, _) -> Nothing
  (Regex @a1 @r1 regex1 children1, Regex @a2 @r2 regex2 children2) -> case (Typeable.eqT @a1 @a2, Typeable.eqT @r1 @r2, regex1 == regex2) of
    (Just Typeable.Refl, Just Typeable.Refl, True) -> Just $ regex @a1 @r1 regex1 $ children1 <> children2
    (_, _, _) -> Nothing
  (Splat @a1 @r1 children1, Splat @a2 @r2 children2) -> case (Typeable.eqT @a1 @a2, Typeable.eqT @r1 @r2) of
    (Just Typeable.Refl, Just Typeable.Refl) -> Just $ splat @a1 @r1 $ children1 <> children2
    (_, _) -> Nothing
  (Route @a1 @r1 children1, Route @a2 @r2 children2) -> case (Typeable.eqT @a1 @a2, Typeable.eqT @r1 @r2) of
    (Just Typeable.Refl, Just Typeable.Refl) -> Just $ route @a1 @r1 $ children1 <> children2
    (_, _) -> Nothing
  (Query @a1 @r1 children1, Query @a2 @r2 children2) -> case (Typeable.eqT @a1 @a2, Typeable.eqT @r1 @r2) of
    (Just Typeable.Refl, Just Typeable.Refl) -> Just $ query @a1 @r1 $ children1 <> children2
    (_, _) -> Nothing
  (Headers @a1 @r1 children1, Headers @a2 @r2 children2) -> case (Typeable.eqT @a1 @a2, Typeable.eqT @r1 @r2) of
    (Just Typeable.Refl, Just Typeable.Refl) -> Just $ headers @a1 @r1 $ children1 <> children2
    (_, _) -> Nothing
  (Body @a1 @r1 children1, Body @a2 @r2 children2) -> case (Typeable.eqT @a1 @a2, Typeable.eqT @r1 @r2) of
    (Just Typeable.Refl, Just Typeable.Refl) -> Just $ body @a1 @r1 $ children1 <> children2
    (_, _) -> Nothing
  (Apply @t1 @r1 tag1 atom1, Apply @t2 @r2 tag2 atom2) -> case (Typeable.eqT @t1 @t2, Typeable.eqT @r1 @r2) of
    (Just Typeable.Refl, Just Typeable.Refl) -> case tag1 == tag2 of
      True -> case atom1 `smush` atom2 of
        Just newAtom -> Just $ apply @t1 @r1 tag1 newAtom
        Nothing -> Nothing
      False -> Nothing
    (_, _) -> Nothing
  (Respond @a1 @r1 children1, Respond @a2 @r2 children2) -> case (Typeable.eqT @a1 @a2, Typeable.eqT @r1 @r2) of
    (Just Typeable.Refl, Just Typeable.Refl) -> Just $ respond @a1 @r1 $ children1 <> children2
    (_, _) -> Nothing
  -- Method is not comparable
  (_, _) -> Nothing

smushes :: [Atom r] -> [Atom r]
smushes [] = []
smushes singleton@[atom] = singleton
smushes (atom1 : atom2 : atoms) = case atom1 `smush` atom2 of
  Just newAtom -> smushes $ newAtom : atoms
  Nothing ->
    List.concat
      [ smushes (atom1 : atoms)
      , smushes (atom2 : atoms)
      , smushes atoms
      ]

argsTest :: Handler '[] IO
argsTest = \request -> do
  return $ Wai.responseLBS HTTP.status200 [] "world"

argsTest1 :: Handler '[Int] IO
argsTest1 = \x -> \request -> do
  return $ Wai.responseLBS HTTP.status200 [] "world"

argsTest2 :: Handler '[Int, Int] IO
argsTest2 = \x -> \y -> \request -> do
  return $ Wai.responseLBS HTTP.status200 [] "world"

match :: forall a (r :: [Type]). (Web.ToHttpApiData a, Eq a, Typeable.Typeable a, Typeable.Typeable r) => a -> [Atom r] -> Atom r
match = Match @a @r

lit :: forall (r :: [Type]). (Typeable.Typeable r) => Text.Text -> [Atom r] -> Atom r
lit = match @Text.Text

param :: forall a (r :: [Type]). (Web.FromHttpApiData a, Typeable.Typeable a, Typeable.Typeable r) => [Atom (r :> a)] -> Atom r
param = Param @a @r

regex :: forall a (r :: [Type]). (Regex.RegexContext Regex.Regex Text.Text a, Typeable.Typeable a, Typeable.Typeable r) => Text.Text -> [Atom (r :> a)] -> Atom r
regex = Regex @a @r

splat :: forall a (r :: [Type]). (Web.FromHttpApiData a, Typeable.Typeable a, Typeable.Typeable r) => [Atom (r :> NonEmpty.NonEmpty a)] -> Atom r
splat = Splat @a @r

route :: forall a (r :: [Type]). (Route.From a, Typeable.Typeable a, Typeable.Typeable r) => [Atom (r :> a)] -> Atom r
route = Route @a @r

query :: forall a (r :: [Type]). (Query.From a, Typeable.Typeable a, Typeable.Typeable r) => [Atom (r :> a)] -> Atom r
query = Query @a @r

headers :: forall a (r :: [Type]). (Headers.From a, Typeable.Typeable a, Typeable.Typeable r) => [Atom (r :> a)] -> Atom r
headers = Headers @a @r

body :: forall a (r :: [Type]). (Body.From a, Typeable.Typeable a, Typeable.Typeable r) => [Atom (r :> a)] -> Atom r
body = Body @a @r

apply :: forall t (r :: [Type]). (Middleware.Tag t, Eq t, Typeable.Typeable t, Typeable.Typeable r) => t -> Atom r -> Atom r
apply = Apply @t @r

scope :: forall a t (r :: [Type]). (Route.From a, Typeable.Typeable a, Middleware.Tag t, Eq t, Typeable.Typeable t, Typeable.Typeable r) => t -> [Atom (r :> a)] -> Atom r
scope tag children = apply @t @r tag $ route @a @r children

respond :: forall a (r :: [Type]). (Response.To a, Typeable.Typeable a, Typeable.Typeable r) => [Atom (r :> a)] -> Atom r
respond = Respond @a @r

method :: forall env (r :: [Type]). (Typeable.Typeable r) => HTTP.StdMethod -> (env Natural.~> IO) -> Handler r env -> Atom r
method = Method @env @r

endpoint ::
  forall a env (r :: [Type]).
  (Route.From a, Typeable.Typeable a, Typeable.Typeable r, Typeable.Typeable (r :> a)) =>
  HTTP.StdMethod ->
  (env Natural.~> IO) ->
  Handler (r :> a) env ->
  Atom r
endpoint stdMethod transformation handler =
  route @a
    [ method @env @(r :> a) stdMethod transformation handler
    ]

myTest = Warp.run 8080 $ test `withDefault` \_ resp -> resp $ Wai.responseLBS HTTP.status404 [] "Not Found..."

test =
  [ lit
      "hello"
      [ lit
          "world"
          [ param @Bool
              [ method HTTP.GET id testHandler1
              , param @Int
                  [ method HTTP.GET id testHandler2
                  , lit
                      "foo"
                      [ method HTTP.POST id testHandler2
                      ]
                  , param @Float
                      [ method HTTP.PUT id \bool1 -> \int2 -> \f3 -> \req -> do
                          return $ Wai.responseLBS HTTP.status200 [] "many args"
                      ]
                  ]
              ]
          ]
      ]
  , lit
      "world"
      [ method HTTP.GET id \req -> do
          return $ Wai.responseLBS HTTP.status200 [] "world"
      , method HTTP.HEAD id \req -> do
          return $ Wai.responseLBS HTTP.status200 [] "dub"
      ]
  , method HTTP.GET id \req -> do
      return $ Wai.responseLBS HTTP.status200 [] "What's up??"
  ]

testHandler1 :: Bool -> Wai.Request -> IO Wai.Response
testHandler1 x request = do
  return $ Wai.responseLBS HTTP.status200 [] $ LBSChar8.pack $ show x

testHandler2 :: Bool -> Int -> Wai.Request -> IO Wai.Response
testHandler2 x y request = do
  return $ Wai.responseLBS HTTP.status200 [] $ LBSChar8.pack $ show x <> show y

data HList (l :: [Type]) where
  HNil :: HList '[]
  HCons :: e -> HList l -> HList (e ': l)

snoc :: forall (l :: [Type]) (e :: Type). HList l -> e -> HList (l :> e)
snoc HNil x = HCons x HNil
snoc (HCons h t) x = HCons h (snoc t x)

fillHandler :: Handler args env -> HList args -> (Wai.Request -> env Wai.Response)
fillHandler handler HNil = handler
fillHandler handler (HCons x xs) = fillHandler (handler x) xs

myFunc :: Wai.Request -> IO Wai.Response
myFunc = fillHandler handlerTest argsTest
 where
  handlerTest :: Handler [Bool, Int, Float] IO
  handlerTest = \b -> \i -> \f -> \req -> do
    undefined

  argsTest :: HList [Bool, Int, Float]
  argsTest = HCons True (HCons 5 (HCons 5.8 HNil))

withDefault :: [Atom '[]] -> Wai.Middleware
withDefault = withDefaultLoop id HNil

withDefaultLoop :: Wai.Middleware -> HList args -> [Atom args] -> Wai.Middleware
withDefaultLoop middleware args tree backup request respond = case tree of
  [] -> backup request respond
  (node : remTree) ->
    case node of
      Match value subTree ->
        case Wai.pathInfo request of
          [] -> withDefaultLoop middleware args remTree backup request respond
          (pathHead : pathTail) ->
            if pathHead == Web.toUrlPiece value
              then do
                let newRequest = request{Wai.pathInfo = pathTail}
                withDefaultLoop middleware args subTree backup newRequest respond
              else withDefaultLoop middleware args remTree backup request respond
      Param @t subTree ->
        case Wai.pathInfo request of
          [] -> withDefaultLoop middleware args remTree backup request respond
          (pathHead : pathTail) ->
            case Web.parseUrlPiece @t pathHead of
              Left _ -> withDefaultLoop middleware args remTree backup request respond
              Right value -> do
                let newRequest = request{Wai.pathInfo = pathTail}
                withDefaultLoop middleware (snoc args value) subTree backup newRequest respond
      Method stdMethod transformation handler ->
        case HTTP.parseMethod $ Wai.requestMethod request of
          Left _ -> withDefaultLoop middleware args remTree backup request respond
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
              else withDefaultLoop middleware args remTree backup request respond

{-
withDefault :: Tree -> Wai.Middleware
withDefault = loop id
  where
    loop :: Wai.Middleware -> Tree -> Wai.Middleware
    loop middleware forest backup request respond = case forest of
      [] -> backup request respond
      (tree : remForest) ->
        case tree of
          Match value subForest ->
            case Wai.pathInfo request of
              [] -> loop middleware remForest backup request respond
              (pathHead : pathTail) ->
                if pathHead == Web.toUrlPiece value
                  then do
                    let newRequest = request {Wai.pathInfo = pathTail}
                    loop middleware subForest backup newRequest respond
                  else loop middleware remForest backup request respond
          Param @t growSubForest ->
            case Wai.pathInfo request of
              [] -> loop middleware remForest backup request respond
              (pathHead : pathTail) ->
                case Web.parseUrlPiece @t pathHead of
                  Left _ -> loop middleware remForest backup request respond
                  Right value -> do
                    key <- Vault.newKey @t
                    let newVault = Vault.insert key value (Wai.vault request)
                        newRequest = request {Wai.pathInfo = pathTail, Wai.vault = newVault}
                    loop middleware (growSubForest $ Secret.Secret key) backup newRequest respond
          Regex @t regex growSubForest -> do
            case Wai.pathInfo request of
              [] -> loop middleware remForest backup request respond
              (pathHead : pathTail) ->
                case pathHead Regex.=~~ regex of
                  Nothing -> loop middleware remForest backup request respond
                  Just value -> do
                    key <- Vault.newKey @t
                    let newVault = Vault.insert key value (Wai.vault request)
                        newRequest = request {Wai.pathInfo = pathTail, Wai.vault = newVault}
                    loop middleware (growSubForest $ Secret.Secret key) backup newRequest respond
          Splat @t growSubForest -> do
            case Wai.pathInfo request of
              [] -> loop middleware remForest backup request respond
              (pathHead : pathTail) -> case Web.parseUrlPiece @t pathHead of
                Left _ -> loop middleware remForest backup request respond
                Right valueHead -> do
                  key <- Vault.newKey @(NonEmpty.NonEmpty t)
                  let valueTail = getValues @t pathTail
                      nonEmptyPath = valueHead NonEmpty.:| valueTail
                      newVault = Vault.insert key nonEmptyPath (Wai.vault request)
                      newRequest = request {Wai.pathInfo = List.drop (List.length valueTail + 1) (Wai.pathInfo request), Wai.vault = newVault}
                  loop middleware (growSubForest $ Secret.Secret key) backup newRequest respond
            where
              getValues :: forall ty. (Web.FromHttpApiData t) => [Text.Text] -> [t]
              getValues [] = []
              getValues (p : ps) = case Web.parseUrlPiece @t p of
                Left _ -> []
                Right v -> v : getValues @t ps
          Route @t route growSubForest -> do
            case Route.parse route $ Wai.pathInfo request of
              (Left _, _) -> loop middleware remForest backup request respond
              (Right value, newPathInfo) -> do
                key <- Vault.newKey @t
                let newVault = Vault.insert key value (Wai.vault request)
                    newRequest = request {Wai.pathInfo = newPathInfo, Wai.vault = newVault}
                loop middleware (growSubForest $ Secret.Secret key) backup newRequest respond
          Method stdMethod transformation handler ->
            case HTTP.parseMethod $ Wai.requestMethod request of
              Left _ -> loop middleware remForest backup request respond
              Right stdMethod' ->
                if stdMethod == stdMethod' && List.null (Wai.pathInfo request)
                  then
                    middleware
                      ( \request' respond' -> do
                          response <- transformation $ handler request'
                          respond' response
                      )
                      request
                      respond
                  else loop middleware remForest backup request respond
          Apply middleware' tree ->
            loop
              (middleware' . middleware)
              (tree : [])
              (loop middleware remForest backup)
              request
              respond
-}
{-
stringify :: Tree -> IO (Atom.Tree String)
stringify [] = return []
stringify (tree:remForest) = case tree of
  Match value subForest -> do
    stringSubForest <- stringify subForest
    stringRemForest <- stringify remForest
    let string = "/" <> (Text.unpack $ Web.toUrlPiece value)
    return ((Tree.Atom string stringSubForest) : stringRemForest)
  Param @t growSubForest -> do
    secret <- Secret.new @t
    stringSubForest <- stringify $ growSubForest secret
    stringRemForest <- stringify remForest
    let string = "/:" <> showType @t
    return ((Tree.Atom string stringSubForest) : stringRemForest)
  Regex @t regex growSubForest -> do
    secret <- Secret.new @t
    stringSubForest <- stringify $ growSubForest secret
    stringRemForest <- stringify remForest
    let string = "/<" <> Text.unpack regex <> ">"
    return ((Tree.Atom string stringSubForest) : stringRemForest)
  Splat @t growSubForest -> do
    secret <- Secret.new @(NonEmpty.NonEmpty ty)
    forest <- mapM $ produce secret
    return $ Tree.Atom ("/*" <> showType @ty) forest
  (Route @ty route produce) = do
    secret <- Secret.new @ty
    forest <- mapM $ produce secret
    return $ Tree.Atom (Text.unpack (Route.rep route)) forest
  (Method m _ _) = do
    return $ Tree.Atom (show m) []
  (Apply _ api) = do
    (Tree.Atom root subTrees) <- api
    return $ Tree.Atom ("(" <> root <> ")") subTrees
-}
{-
forest :: Tree -> IO (Tree.Atom String)
forest [] = return $ Tree.Atom ":root:" []
forest apis = do
  forest' <- mapM tree apis
  return $ Tree.Atom "\ESC[31m:root:\ESC[0m" forest'
  where
    tree :: Atom -> IO (Tree.Atom String)
    tree (Match value apis) = do
      forest <- mapM tree apis
      return $ Tree.Atom ("/" <> (Text.unpack $ Web.toUrlPiece value)) forest
    tree (Param @ty produce) = do
      secret <- Secret.new @ty
      forest <- mapM tree $ produce secret
      return $ Tree.Atom ("/:" <> showType @ty) forest
    tree (Regex @ty regex produce) = do
      secret <- Secret.new @ty
      forest <- mapM tree $ produce secret
      return $ Tree.Atom ("/r<" <> Text.unpack regex <> ">") forest
    tree (Splat @ty produce) = do
      secret <- Secret.new @(NonEmpty.NonEmpty ty)
      forest <- mapM tree $ produce secret
      return $ Tree.Atom ("/*" <> showType @ty) forest
    tree (Route @ty route produce) = do
      secret <- Secret.new @ty
      forest <- mapM tree $ produce secret
      return $ Tree.Atom (Text.unpack (Route.rep route)) forest
    tree (Method m _ _) = do
      return $ Tree.Atom (show m) []
    tree (Apply _ api) = do
      (Tree.Atom root subTrees) <- tree api
      return $ Tree.Atom ("(" <> root <> ")") subTrees

showType :: forall a. (Typeable.Typeable a) => String
showType = show . Typeable.typeRep $ Typeable.Proxy @a

get_ = method HTTP.GET

getIO_ = method HTTP.GET id
-}
