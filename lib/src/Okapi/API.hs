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

module Okapi.API where

import Control.Natural qualified as Natural
import Data.Binary.Builder qualified as Builder
import Data.ByteString.Lazy qualified as LBS
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
import Okapi.Headers qualified as Headers
import Okapi.Response qualified as Response
import Okapi.Route qualified as Route
import Okapi.Secret qualified as Secret
import Web.HttpApiData qualified as Web

type Handler env = Wai.Request -> env Wai.Response

data API where
  Match :: forall a. (Web.ToHttpApiData a) => a -> [API] -> API
  Param :: forall a. (Web.FromHttpApiData a, Typeable.Typeable a) => (Secret.Secret a -> [API]) -> API
  Regex :: API
  Splat :: forall a. (Web.FromHttpApiData a) => (Secret.Secret (NonEmpty.NonEmpty a) -> [API]) -> API
  Route :: forall a. Route.Parser a -> (Secret.Secret a -> [API]) -> API
  Method :: forall env. HTTP.StdMethod -> (env Natural.~> IO) -> Handler env -> API
  -- Query :: forall a. Query.Parser a -> (Secret.Secret a -> [API]) -> API
  -- Headers :: forall a. RequestHeaders.Parser a -> (Secret.Secret a -> [API]) -> API
  -- Body :: forall a. RequestBody.Parser a -> (Secret.Secret a -> [API]) -> API
  Pipe :: Wai.Middleware -> API -> API
  Respond ::
    forall (status :: Natural.Natural) (headerKeys :: [Exts.Symbol]) (contentType :: Type) (resultType :: Type).
    (Response.ToContentType contentType resultType) =>
    ((Response.Headers headerKeys -> resultType -> Wai.Response) -> [API]) ->
    API

match :: forall a. (Web.ToHttpApiData a) => a -> [API] -> API
match = Match

lit :: Text.Text -> [API] -> API
lit = match @Text.Text

param :: forall a. (Web.FromHttpApiData a, Typeable.Typeable a) => (Secret.Secret a -> [API]) -> API
param = Param

splat :: forall a. (Web.FromHttpApiData a) => (Secret.Secret (NonEmpty.NonEmpty a) -> [API]) -> API
splat = Splat

route :: forall a. Route.Parser a -> (Secret.Secret a -> [API]) -> API
route = Route

pipe :: Wai.Middleware -> API -> API
pipe = Pipe

scope :: Wai.Middleware -> Text.Text -> [API] -> API
scope mw t apps = pipe mw $ lit t apps

method :: forall env. HTTP.StdMethod -> (env Natural.~> IO) -> Handler env -> API
method = Method

respond ::
  forall (status :: Natural.Natural) (headerKeys :: [Exts.Symbol]) (contentType :: Type) (resultType :: Type).
  (Response.ToContentType contentType resultType) =>
  ((Response.Headers headerKeys -> resultType -> Wai.Response) -> [API]) ->
  API
respond = Respond

build :: [API] -> Wai.Middleware -> Wai.Middleware
build [] _ backup req resp = backup req resp
build (api : apis) middlewareToApply backup req resp =
  case api of
    Match value children ->
      case Wai.pathInfo req of
        [] -> build apis middlewareToApply backup req resp
        (pathHead : pathTail) ->
          if pathHead == Web.toUrlPiece value
            then do
              let newReq = req {Wai.pathInfo = pathTail}
              build children middlewareToApply backup newReq resp
            else build apis middlewareToApply backup req resp
    Param @ty produce ->
      case Wai.pathInfo req of
        [] -> build apis middlewareToApply backup req resp
        (pathHead : pathTail) ->
          case Web.parseUrlPiece @ty pathHead of
            Left _ -> build apis middlewareToApply backup req resp
            Right value -> do
              key <- Vault.newKey @ty
              let newVault = Vault.insert key value (Wai.vault req)
                  newReq = req {Wai.pathInfo = pathTail, Wai.vault = newVault}
              build (produce $ Secret.Secret key) middlewareToApply backup newReq resp
    Splat @ty produce -> do
      case Wai.pathInfo req of
        [] -> build apis middlewareToApply backup req resp
        (pathHead : pathTail) -> case Web.parseUrlPiece @ty pathHead of
          Left _ -> build apis middlewareToApply backup req resp
          Right valueHead -> do
            -- TODO: FIX ALGORITHM!
            let valueTail = loop @ty pathTail
                nonEmptyPath = valueHead NonEmpty.:| valueTail
            key <- Vault.newKey @(NonEmpty.NonEmpty ty)
            let newVault = Vault.insert key nonEmptyPath (Wai.vault req)
                newReq = req {Wai.pathInfo = List.drop (List.length valueTail + 1) (Wai.pathInfo req), Wai.vault = newVault}
            build (produce $ Secret.Secret key) middlewareToApply backup newReq resp
      where
        loop :: forall ty. (Web.FromHttpApiData ty) => [Text.Text] -> [ty]
        loop [] = []
        loop (t : ts) = case Web.parseUrlPiece @ty t of
          Left _ -> []
          Right v -> v : loop @ty ts
    Route @ty route produce -> do
      case Route.exec route $ Wai.pathInfo req of
        (Left _, _) -> build apis middlewareToApply backup req resp
        (Right value, newPathInfo) -> do
          key <- Vault.newKey @ty
          let newVault = Vault.insert key value (Wai.vault req)
              newReq = req {Wai.pathInfo = newPathInfo, Wai.vault = newVault}
          build (produce $ Secret.Secret key) middlewareToApply backup newReq resp
    Method m trans handler ->
      case HTTP.parseMethod $ Wai.requestMethod req of
        Left _ -> build apis middlewareToApply backup req resp
        Right stdMethod ->
          if m == stdMethod && List.null (Wai.pathInfo req)
            then
              middlewareToApply
                ( \req' resp' -> do
                    res <- trans $ handler req'
                    resp' res
                )
                req
                resp
            else build apis middlewareToApply backup req resp
    Pipe otherMiddlewareToApply app ->
      build
        (app : [])
        (otherMiddlewareToApply . middlewareToApply)
        (build apis middlewareToApply backup)
        req
        resp

forest :: [API] -> IO (Tree.Tree String)
forest [] = return $ Tree.Node ":root:" []
forest apis = do
  forest' <- mapM tree apis
  return $ Tree.Node "\ESC[31m:root:\ESC[0m" forest'
  where
    tree :: API -> IO (Tree.Tree String)
    tree (Match value apis) = do
      forest <- mapM tree apis
      return $ Tree.Node ("/" <> (Text.unpack $ Web.toUrlPiece value)) forest
    tree (Param @ty produce) = do
      secret <- Secret.new @ty
      forest <- mapM tree $ produce secret
      return $ Tree.Node ("/:" <> showType @ty) forest
    tree (Splat @ty produce) = do
      secret <- Secret.new @(NonEmpty.NonEmpty ty)
      forest <- mapM tree $ produce secret
      return $ Tree.Node "/*" forest
    tree (Route @ty route produce) = do
      secret <- Secret.new @ty
      forest <- mapM tree $ produce secret
      return $ Tree.Node (Text.unpack (Route.rep route)) forest
    tree (Method m _ _) = do
      return $ Tree.Node (show m) []
    tree (Pipe _ api) = do
      (Tree.Node root subTrees) <- tree api
      return $ Tree.Node ("(" <> root <> ")") subTrees

showType :: forall a. (Typeable.Typeable a) => String
showType = show . Typeable.typeRep $ Typeable.Proxy @a

get_ = method HTTP.GET

getIO_ = method HTTP.GET id