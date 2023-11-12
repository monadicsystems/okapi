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
    forall (status :: Natural.Natural) (headerKeys :: [Exts.Symbol]) (contentType :: Type) (resultType :: Type) (r :: [Type]).
    ( Response.ContentType contentType
    , Response.ToContentType contentType resultType
    , Typeable.Typeable status
    , Typeable.Typeable headerKeys
    , Typeable.Typeable contentType
    , Typeable.Typeable resultType
    , Typeable.Typeable r
    ) =>
    Node (r :-> (Response.Headers headerKeys -> resultType -> Wai.Response)) ->
    Node r
  Method ::
    forall env (r :: [Type]).
    (Typeable.Typeable r) =>
    HTTP.StdMethod ->
    (env Natural.~> IO) ->
    Handler r env ->
    Node r

{-
smush ::
  forall (r :: [Type]).
  (Typeable.Typeable r) =>
  Node r ->
  Node r ->
  Maybe (Node r)
smush a1 a2 = case (a1, a2) of
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
  (Apply @t1 @r1 tag1 atom1, Apply @t2 @r2 tag2 atom2) -> case (Typeable.eqT @t1 @t2, Typeable.eqT @r1 @r2) of
    (Just Typeable.Refl, Just Typeable.Refl) -> case tag1 == tag2 of
      True -> Just $ apply @t1 @r1 tag1 $ choice @r1 [atom1, atom2]
      False -> Nothing
    (_, _) -> Nothing
  (Responder @s1 @hk1 @ct1 @rt1 @r1 child1, Responder @s2 @hk2 @ct2 @rt2 @r2 child2) -> case (Typeable.eqT @s1 @s2, Typeable.eqT @hk1 @hk2, Typeable.eqT @ct1 @ct2, Typeable.eqT @rt1 @rt2, Typeable.eqT @r1 @r2) of
    (Just Typeable.Refl, Just Typeable.Refl, Just Typeable.Refl, Just Typeable.Refl, Just Typeable.Refl) -> Just $ responder @s1 @hk1 @ct1 @rt1 @r1 $ choice @(r1 :-> (Response.Headers hk1 -> rt1 -> Wai.Response)) [child1, child2]
    (_, _, _, _, _) -> Nothing
  (Choice @r1 children, a2') -> Just $ choice @r1 (children <> [a2'])
  (a1', Choice @r2 children) -> Just $ choice @r2 (a1' : children)
  -- Method is not comparable
  (_, _) -> Nothing
-}
-- smushes :: Node r -> Node r
-- smushes (Choice []) = Choice []
-- smushes (Choice singleton@[atom]) = Choice singleton
-- smushes (Choice (atom1 : atom2 : atoms)) = case atom1 `smush` atom2 of
--   Just newAtom -> smushes $ Choice (newAtom : atoms)
--   Nothing ->
--     List.concat
--       [ smushes (atom1 : atoms)
--       , smushes (atom2 : atoms)
--       , smushes atoms
--       ]
-- smushes atom = atom

argsTest :: Handler '[] IO
argsTest = \request -> do
  return $ Wai.responseLBS HTTP.status200 [] "world"

argsTest1 :: Handler '[Int] IO
argsTest1 = \x -> \request -> do
  return $ Wai.responseLBS HTTP.status200 [] "world"

argsTest2 :: Handler '[Int, Int] IO
argsTest2 = \x -> \y -> \request -> do
  return $ Wai.responseLBS HTTP.status200 [] "world"

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
  forall (status :: Natural.Natural) (headerKeys :: [Exts.Symbol]) (contentType :: Type) (resultType :: Type) (r :: [Type]).
  ( Response.ContentType contentType
  , Response.ToContentType contentType resultType
  , Typeable.Typeable status
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

myTest =
  Warp.run 8081 $ test `withDefault` \_ resp ->
    resp $ Wai.responseLBS HTTP.status404 [] "Not Found..."

myTest2 =
  Warp.run 8082 $ test2 `withDefault` \_ resp ->
    resp $ Wai.responseLBS HTTP.status404 [] "Not Found..."

data HelloWorldBody = HelloWorldBody deriving (Typeable.Typeable)

data ByeWorldBody = ByeWorldBody {error :: Text.Text, randomN :: Int} deriving (Typeable.Typeable)

textToBytes :: Text.Text -> LBSChar8.ByteString
textToBytes = undefined

instance Response.ContentType Text.Text where
  contentTypeName = "text/plain"
  contentTypeBody text = Response.BodyBytes $ textToBytes text

instance Response.ToContentType Text.Text HelloWorldBody where
  toContentType HelloWorldBody = "Hello World! :)"

instance Response.ToContentType Text.Text ByeWorldBody where
  toContentType (ByeWorldBody _error _randomN) = "Bye World... :("

-- test :: _
test =
  myResponders
    . lit "some"
    . lit "world"
    . param @Float
    $ method
      HTTP.GET
      id
      \helloWorld byeWorld float req -> do
        undefined

test2 =
  choice
    [ lit "some"
        . lit "path"
        $ choice
          [ method HTTP.GET id \req -> do
              undefined
          , method HTTP.POST id \req -> do
              undefined
          ]
    , param @Float
        $ method HTTP.PUT id \req -> do
          undefined
    , choice
        [ lit "lol"
            . lit "foo"
            . param @Int
            $ method HTTP.GET id \n req -> do
              undefined
        , lit "bar"
            . match @Int 10
            $ method HTTP.PATCH id \req -> do
              undefined
        , lit "baz"
            $ choice
              [ method HTTP.GET id \req -> do
                  undefined
              , param @Float $ method HTTP.PUT id \f req -> do
                  undefined
              ]
        ]
    ]

myResponders =
  responder @200 @'["HELLO-HEADER"] @Text.Text @HelloWorldBody
    . responder @204 @'["BYE-HEADER"] @Text.Text @ByeWorldBody

testHandler1 :: Bool -> Wai.Request -> IO Wai.Response
testHandler1 x request = do
  return $ Wai.responseLBS HTTP.status200 [] $ LBSChar8.pack $ show x

testHandler2 :: Bool -> Int -> Wai.Request -> IO Wai.Response
testHandler2 x y request = do
  return $ Wai.responseLBS HTTP.status200 [] $ LBSChar8.pack $ show x <> show y

data HList (l :: [Type]) where
  HNil :: HList '[]
  HCons :: e -> HList l -> HList (e ': l)

snoc :: forall (l :: [Type]) (e :: Type). HList l -> e -> HList (l :-> e)
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

withDefault :: Node '[] -> Wai.Middleware
withDefault = undefined

-- withDefaultLoop id HNil

withDefaultLoop :: Wai.Middleware -> HList args -> Node args -> Wai.Middleware
withDefaultLoop = undefined

{-
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
      Responder @s @hk @ct @rt @r child ->
        let callback = Response.makeResponder @s @hk @ct @rt
         in withDefaultLoop
              middleware
              (snoc args callback)
              (child : [])
              (withDefaultLoop middleware args remTree backup)
              request
              respond
-}
---- TODO: May need system for content-type negotiation???
---- The accepted content types can be the same or more
---- If Accept is less than the responseses content types, then I can't go down that tree

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
