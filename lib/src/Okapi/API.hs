{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
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
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Okapi.API where

import Control.Natural qualified as Natural
import Data.Functor.Identity qualified as Identity
import Data.List qualified as List
import Data.List.NonEmpty qualified as NonEmpty
import Data.Text qualified as Text
import Data.Tree qualified as Tree
import Data.Typeable qualified as Typeable
import Data.Vault.Lazy qualified as Vault
import Network.HTTP.Types qualified as HTTP
import Network.Wai qualified as Wai
import Okapi.Headers qualified as Headers
import Okapi.Route qualified as Route
import Okapi.Secret qualified as Secret
import Web.HttpApiData qualified as Web

type Handler env = Wai.Request -> env Wai.Response

data API where
  Match :: Text.Text -> [API] -> API
  Param :: forall a. (Web.FromHttpApiData a, Typeable.Typeable a) => (Secret.Secret a -> [API]) -> API
  Splat :: (Secret.Secret (NonEmpty.NonEmpty Text.Text) -> [API]) -> API
  Router :: forall a. Route.Route a -> (Secret.Secret a -> [API]) -> API
  Meta :: Headers.Headers a -> (Secret.Secret a -> [API]) -> API
  Method :: forall env. (HTTP.StdMethod -> Bool) -> (env Natural.~> IO) -> Handler env -> API
  Wrap :: Wai.Middleware -> API -> API

match :: Text.Text -> [API] -> API
match = Match

lit :: forall a. (Web.ToHttpApiData a) => a -> [API] -> API
lit l = Match (Web.toUrlPiece l)

param :: forall a. (Web.FromHttpApiData a, Typeable.Typeable a) => (Secret.Secret a -> [API]) -> API
param = Param

splat :: (Secret.Secret (NonEmpty.NonEmpty Text.Text) -> [API]) -> API
splat = Splat

router :: forall a. Route.Route a -> (Secret.Secret a -> [API]) -> API
router = Router

meta :: forall a. Headers.Headers a -> (Secret.Secret a -> [API]) -> API
meta = Meta

wrap :: Wai.Middleware -> API -> API
wrap = Wrap

scope :: Wai.Middleware -> Text.Text -> [API] -> API
scope mw t apps = wrap mw $ match t apps

method_ :: forall env. (HTTP.StdMethod -> Bool) -> (env Natural.~> IO) -> Handler env -> API
method_ = Method

get_ :: forall env. (env Natural.~> IO) -> Handler env -> API
get_ = method_ (HTTP.GET ==)

getIO_ :: Handler IO -> API
getIO_ = get_ id

getPure_ :: Handler Identity.Identity -> API
getPure_ = get_ (return . Identity.runIdentity)

post_ :: forall env. (env Natural.~> IO) -> Handler env -> API
post_ = method_ (HTTP.POST ==)

postIO_ :: Handler IO -> API
postIO_ = post_ id

postPure_ :: Handler Identity.Identity -> API
postPure_ = post_ (return . Identity.runIdentity)

head_ :: forall env. (env Natural.~> IO) -> Handler env -> API
head_ = method_ (HTTP.HEAD ==)

headIO_ :: Handler IO -> API
headIO_ = head_ id

headPure_ :: Handler Identity.Identity -> API
headPure_ = head_ (return . Identity.runIdentity)

put_ :: forall env. (env Natural.~> IO) -> Handler env -> API
put_ = method_ (HTTP.PUT ==)

putIO_ :: Handler IO -> API
putIO_ = put_ id

putPure_ :: Handler Identity.Identity -> API
putPure_ = put_ (return . Identity.runIdentity)

delete_ :: forall env. (env Natural.~> IO) -> Handler env -> API
delete_ = method_ (HTTP.DELETE ==)

deleteIO_ :: Handler IO -> API
deleteIO_ = delete_ id

deletePure_ :: Handler Identity.Identity -> API
deletePure_ = delete_ (return . Identity.runIdentity)

trace_ :: forall env. (env Natural.~> IO) -> Handler env -> API
trace_ = method_ (HTTP.TRACE ==)

traceIO_ :: Handler IO -> API
traceIO_ = trace_ id

tracePure_ :: Handler Identity.Identity -> API
tracePure_ = trace_ (return . Identity.runIdentity)

connect_ :: forall env. (env Natural.~> IO) -> Handler env -> API
connect_ = method_ (HTTP.CONNECT ==)

connectIO_ :: Handler IO -> API
connectIO_ = connect_ id

connectPure_ :: Handler Identity.Identity -> API
connectPure_ = connect_ (return . Identity.runIdentity)

options_ :: forall env. (env Natural.~> IO) -> Handler env -> API
options_ = method_ (HTTP.OPTIONS ==)

optionsIO_ :: Handler IO -> API
optionsIO_ = options_ id

optionsPure_ :: Handler Identity.Identity -> API
optionsPure_ = options_ (return . Identity.runIdentity)

patch_ :: forall env. (env Natural.~> IO) -> Handler env -> API
patch_ = method_ (HTTP.PATCH ==)

patchIO_ :: Handler IO -> API
patchIO_ = patch_ id

patchPure_ :: Handler Identity.Identity -> API
patchPure_ = patch_ (return . Identity.runIdentity)

any_ :: forall env. (env Natural.~> IO) -> Handler env -> API
any_ = method_ (const True)

build :: [API] -> Wai.Middleware -> Wai.Middleware
build [] _ backup req resp = backup req resp
build (api : apis) middlewareToApply backup req resp =
  case api of
    Match text children ->
      case Wai.pathInfo req of
        [] -> build apis middlewareToApply backup req resp
        (pathHead : pathTail) ->
          if pathHead == text
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
    Splat produce -> do
      case Wai.pathInfo req of
        [] -> build apis middlewareToApply backup req resp
        (pathHead : pathTail) -> do
          let nonEmptyPath = pathHead NonEmpty.:| pathTail
          key <- Vault.newKey @(NonEmpty.NonEmpty Text.Text)
          let newVault = Vault.insert key nonEmptyPath (Wai.vault req)
              newReq = req {Wai.pathInfo = [], Wai.vault = newVault}
          build (produce $ Secret.Secret key) middlewareToApply backup newReq resp
    Router @ty route produce -> do
      case Route.exec route $ Wai.pathInfo req of
        (Left _, _) -> build apis middlewareToApply backup req resp
        (Right value, newPathInfo) -> do
          key <- Vault.newKey @ty
          let newVault = Vault.insert key value (Wai.vault req)
              newReq = req {Wai.pathInfo = newPathInfo, Wai.vault = newVault}
          build (produce $ Secret.Secret key) middlewareToApply backup newReq resp
    Method pred trans handler ->
      case HTTP.parseMethod $ Wai.requestMethod req of
        Left _ -> build apis middlewareToApply backup req resp
        Right stdMethod ->
          if pred stdMethod && List.null (Wai.pathInfo req)
            then
              middlewareToApply
                ( \req' resp' -> do
                    res <- trans $ handler req'
                    resp' res
                )
                req
                resp
            else build apis middlewareToApply backup req resp
    Wrap otherMiddlewareToApply app ->
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
  return $ Tree.Node ":root:" forest'

tree :: API -> IO (Tree.Tree String)
tree (Match text apis) = do
  forest <- mapM tree apis
  return $ Tree.Node ("/" <> Text.unpack text) forest
tree (Param @ty produce) = do
  secret <- Secret.new @ty
  forest <- mapM tree $ produce secret
  return $ Tree.Node ("/:" <> showType @ty) forest
  where
    showType :: forall a. (Typeable.Typeable a) => String
    showType = show . Typeable.typeRep $ Typeable.Proxy @a
tree (Splat produce) = do
  secret <- Secret.new @(NonEmpty.NonEmpty Text.Text)
  forest <- mapM tree $ produce secret
  return $ Tree.Node "/*" forest
tree (Router @ty route produce) = do
  secret <- Secret.new @ty
  forest <- mapM tree $ produce secret
  return $ Tree.Node (Text.unpack (Route.rep route)) forest
tree (Method pred _ _) = do
  return $ Tree.Node (show $ filter pred [minBound ..]) []
tree (Wrap _ api) = do
  (Tree.Node root subTrees) <- tree api
  return $ Tree.Node ("---" <> root <> "->>") subTrees
