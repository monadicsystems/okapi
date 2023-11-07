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
import Okapi.Headers qualified as Headers
import Okapi.Response qualified as Response
import Okapi.Route qualified as Route
import Okapi.Secret qualified as Secret
import Text.Regex.TDFA qualified as Regex
import Web.HttpApiData qualified as Web

type Tree r = [Node r]

type (:>) :: [Type] -> Type -> [Type]
type family (:>) (a :: [Type]) (b :: Type) where
  (:>) '[] b = '[b]
  (:>) (aa : aas) b = aa : (aas :> b)

data Node (r :: [Type]) where
  Match :: forall a (r :: [Type]). (Web.ToHttpApiData a) => a -> Tree r -> Node r
  Param :: forall a (r :: [Type]). (Web.FromHttpApiData a, Typeable.Typeable a) => Tree (r :> a) -> Node r
  -- Regex :: forall a (r :: [Type]). (Regex.RegexContext Regex.Regex Text.Text a) => Text.Text -> Tree (a : r) -> Node (a : r)
  -- Splat :: forall a (r :: [Type]). (Web.FromHttpApiData a, Typeable.Typeable a) => Tree (NonEmpty.NonEmpty a : r) -> Node (NonEmpty.NonEmpty a : r)
  -- Route :: forall a (r :: [Type]). Route.Parser a -> Tree (a : r) -> Node (a : r)
  Method :: forall env (r :: [Type]). HTTP.StdMethod -> (env Natural.~> IO) -> Handler r env -> Node r

-- Query :: forall a. Query.Parser a -> (Secret.Secret a -> Tree) -> Node
-- Headers :: forall a. RequestHeaders.Parser a -> (Secret.Secret a -> Tree) -> Node
-- Body :: forall a. RequestBody.Parser a -> (Secret.Secret a -> Tree) -> Node
-- Apply :: forall (r :: [Type]). Wai.Middleware -> Node r -> Node r

type Handler :: [Type] -> (Type -> Type) -> Type
type family Handler args env where
  Handler '[] env = Wai.Request -> env Wai.Response
  Handler (arg : args) env = arg -> Handler args env

argsTest :: Handler '[] IO
argsTest = \request -> do
  return $ Wai.responseLBS HTTP.status200 [] "world"

argsTest1 :: Handler '[Int] IO
argsTest1 = \x -> \request -> do
  return $ Wai.responseLBS HTTP.status200 [] "world"

argsTest2 :: Handler '[Int, Int] IO
argsTest2 = \x -> \y -> \request -> do
  return $ Wai.responseLBS HTTP.status200 [] "world"

match :: forall a (r :: [Type]). (Web.ToHttpApiData a) => a -> Tree r -> Node r
match = Match

lit :: forall (r :: [Type]). Text.Text -> Tree r -> Node r
lit = match @Text.Text

param :: forall a (r :: [Type]). (Web.FromHttpApiData a, Typeable.Typeable a) => Tree (r :> a) -> Node r
param = Param @a @r

method :: forall env (r :: [Type]). HTTP.StdMethod -> (env Natural.~> IO) -> Handler r env -> Node r
method = Method

type Root = '[]

myTest = Warp.run 8080 $ test `withDefault` \_ resp -> resp $ Wai.responseLBS HTTP.status404 [] "Not Found..."

test :: Tree Root
test =
  [ lit
      "hello"
      [ lit
          "world"
          [ param @Bool
              [ method HTTP.GET id testHandler1,
                param @Int
                  [ method HTTP.GET id testHandler2,
                    lit
                      "foo"
                      [ method HTTP.POST id testHandler2
                      ],
                    param @Float
                      [ method HTTP.PUT id \bool1 -> \int2 -> \f3 -> \req -> do
                          return $ Wai.responseLBS HTTP.status200 [] "many args"
                      ]
                  ]
              ]
          ]
      ],
    lit
      "world"
      [ method HTTP.GET id \req -> do
          return $ Wai.responseLBS HTTP.status200 [] "world",
        method HTTP.HEAD id \req -> do
          return $ Wai.responseLBS HTTP.status200 [] "dub"
      ],
    method HTTP.GET id \req -> do
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

-- type Reverse :: [Type] -> [Type]
-- type family Reverse l where
--   Reverse '[] = '[]
--   Reverse (h : t) = Reverse t :> h

fillHandler :: Handler args env -> HList args -> (Wai.Request -> env Wai.Response)
fillHandler handler HNil = handler
fillHandler handler (HCons x xs) = fillHandler (handler x) xs

myFunc :: Wai.Request -> IO Wai.Response
myFunc = fillHandler handlerTest argsTest
  where
    handlerTest :: Handler [Bool, Int, Float] IO
    handlerTest = \b -> \i -> \f -> \req -> do
      undefined

    argsTest :: HList '[Bool, Int, Float]
    argsTest = HCons True (HCons 5 (HCons 5.8 HNil))

withDefault :: Tree Root -> Wai.Middleware
withDefault = withDefaultLoop id HNil

withDefaultLoop :: Wai.Middleware -> HList args -> Tree args -> Wai.Middleware
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
                let newRequest = request {Wai.pathInfo = pathTail}
                withDefaultLoop middleware args subTree backup newRequest respond
              else withDefaultLoop middleware args remTree backup request respond
      Param @t subTree ->
        case Wai.pathInfo request of
          [] -> withDefaultLoop middleware args remTree backup request respond
          (pathHead : pathTail) ->
            case Web.parseUrlPiece @t pathHead of
              Left _ -> withDefaultLoop middleware args remTree backup request respond
              Right value -> do
                let newRequest = request {Wai.pathInfo = pathTail}
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
type Remove :: Exts.Symbol -> [Exts.Symbol] -> [Exts.Symbol]
type family Remove x ys where
  Remove a '[] = '[]
  Remove a (a ': ys) = ys
  Remove a (y ': ys) = y ': (Remove a ys)
-}

{-
data Node (r :: [Type]) where
  Match :: forall a (r :: [Type]). (Web.ToHttpApiData a) => a -> Tree r -> Node r
  Param :: forall a. (Web.FromHttpApiData a, Typeable.Typeable a) => (Secret.Secret a -> Tree) -> Node
  Regex :: forall a. (Regex.RegexContext Regex.Regex Text.Text a) => Text.Text -> (Secret.Secret a -> Tree) -> Node
  Splat :: forall a. (Web.FromHttpApiData a, Typeable.Typeable a) => (Secret.Secret (NonEmpty.NonEmpty a) -> Tree) -> Node
  Route :: forall a. Route.Parser a -> (Secret.Secret a -> Tree) -> Node
  Method :: forall env. HTTP.StdMethod -> (env Natural.~> IO) -> Handler env -> Node
  -- Query :: forall a. Query.Parser a -> (Secret.Secret a -> Tree) -> Node
  -- Headers :: forall a. RequestHeaders.Parser a -> (Secret.Secret a -> Tree) -> Node
  -- Body :: forall a. RequestBody.Parser a -> (Secret.Secret a -> Tree) -> Node
  Apply :: Wai.Middleware -> Node -> Node
-}

-- Respond ::
--   forall (status :: Natural.Natural) (headerKeys :: [Exts.Symbol]) (contentType :: Type) (resultType :: Type).
--   (Response.ToContentType contentType resultType) =>
--   ((Response.Headers headerKeys -> resultType -> Wai.Response) -> Tree) ->
--   Node

{-
regex :: forall a. (Regex.RegexContext Regex.Regex Text.Text a) => Text.Text -> (Secret.Secret a -> Tree) -> Node
regex = Regex

splat :: forall a. (Web.FromHttpApiData a, Typeable.Typeable a) => (Secret.Secret (NonEmpty.NonEmpty a) -> Tree) -> Node
splat = Splat

route :: forall a. Route.Parser a -> (Secret.Secret a -> Tree) -> Node
route = Route

apply :: Wai.Middleware -> Node -> Node
apply = Apply

scope :: Wai.Middleware -> Text.Text -> Tree -> Node
scope mw t forest = apply mw $ lit t forest
-}

{-
-- respond ::
--   forall (status :: Natural.Natural) (headerKeys :: [Exts.Symbol]) (contentType :: Type) (resultType :: Type).
--   (Response.ToContentType contentType resultType) =>
--   ((Response.Headers headerKeys -> resultType -> Wai.Response) -> Tree) ->
--   Node
-- respond = Respond

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
{-
data Node where
  Match :: forall a. (Web.ToHttpApiData a) => a -> Tree -> Node
  Param :: forall a. (Web.FromHttpApiData a, Typeable.Typeable a) => (Secret.Secret a -> Tree) -> Node
  Regex :: forall a. (Regex.RegexContext Regex.Regex Text.Text a) => Text.Text -> (Secret.Secret a -> Tree) -> Node
  Splat :: forall a. (Web.FromHttpApiData a, Typeable.Typeable a) => (Secret.Secret (NonEmpty.NonEmpty a) -> Tree) -> Node
  Route :: forall a. Route.Parser a -> (Secret.Secret a -> Tree) -> Node
  Method :: forall env. HTTP.StdMethod -> (env Natural.~> IO) -> Handler env -> Node
  -- Query :: forall a. Query.Parser a -> (Secret.Secret a -> Tree) -> Node
  -- Headers :: forall a. RequestHeaders.Parser a -> (Secret.Secret a -> Tree) -> Node
  -- Body :: forall a. RequestBody.Parser a -> (Secret.Secret a -> Tree) -> Node
  Pipe :: Wai.Middleware -> Node -> Node
  Respond ::
    forall (status :: Natural.Natural) (headerKeys :: [Exts.Symbol]) (contentType :: Type) (resultType :: Type).
    (Response.ToContentType contentType resultType) =>
    ((Response.Headers headerKeys -> resultType -> Wai.Response) -> Tree) ->
    Node

-- data AppF a where
--   MatchF :: forall a b. (Web.ToHttpApiData b) => b -> [a] -> AppF a
--   ParamF :: forall a b. (Web.FromHttpApiData b, Typeable.Typeable b) => (Secret.Secret b -> [a]) -> AppF a
--   RegexF :: forall a b. (Regex.RegexContext Regex.Regex Text.Text b) => Text.Text -> (Secret.Secret b -> [a]) -> AppF a
--   SplatF :: forall a b. (Web.FromHttpApiData b, Typeable.Typeable b) => (Secret.Secret (NonEmpty.NonEmpty b) -> [a]) -> AppF a
--   RouteF :: forall a b. Route.Parser b -> (Secret.Secret b -> [a]) -> AppF a
--   MethodF :: forall a env. HTTP.StdMethod -> (env Natural.~> IO) -> Handler env -> AppF a
--   -- Query :: forall a. Query.Parser a -> (Secret.Secret a -> [AppF]) -> AppF
--   -- Headers :: forall a. RequestHeaders.Parser a -> (Secret.Secret a -> [AppF]) -> AppF
--   -- Body :: forall a. RequestBody.Parser a -> (Secret.Secret a -> [AppF]) -> AppF
--   PipeF :: forall a. Wai.Middleware -> a -> AppF a
  -- RespondF ::
  --   forall (status :: Natural.Natural) (headerKeys :: [Exts.Symbol]) (contentType :: Type) (resultType :: Type).
  --   (Response.ToContentType contentType resultType) =>
  --   ((Response.Headers headerKeys -> resultType -> Wai.Response) -> [a]) ->
  --   AppF a

build' :: Tree -> Wai.Middleware -> Wai.Middleware
build' root middleware backup request respond = Foldable.fold
  (\case
      Base.Nil -> backup request respond
      Base.Cons api (apis :: _) -> case api of
        Match value children ->
          case Wai.pathInfo request of
            [] -> build' apis middleware backup request respond
            (pathHead : pathTail) ->
              if pathHead == Web.toUrlPiece value
                then do
                  let newReq = request {Wai.pathInfo = pathTail}
                  build' children middleware backup newReq respond
                else build' apis middleware backup request respond
  )
  root

endpoint ::
  HTTP.StdMethod ->
  Route.Parser a ->
  (env Natural.~> IO) ->
  (Secret.Secret a -> Handler env) ->
  Node
endpoint stdMethod routeP trans handlerWithSecret = route routeP \routeS ->
  [ method stdMethod trans (handlerWithSecret routeS)
  ]
-}