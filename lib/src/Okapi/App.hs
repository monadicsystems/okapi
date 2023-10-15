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

module Okapi.App where

import Control.Natural qualified as Natural
import Data.Functor.Identity qualified as Identity
import Data.List qualified as List
import Data.Text qualified as Text
import Data.Tree qualified as Tree
import Data.Typeable qualified as Typeable
import Data.Vault.Lazy qualified as Vault
import Network.HTTP.Types qualified as HTTP
import Network.Wai qualified as Wai
import Okapi.Route qualified as Route
import Okapi.Secret qualified as Secret
import Web.HttpApiData qualified as Web

type Handler env = Wai.Request -> env Wai.Response

data App where
  Match :: Text.Text -> [App] -> App
  Capture :: forall a. (Web.FromHttpApiData a, Typeable.Typeable a) => (Secret.Secret a -> [App]) -> App
  Splat :: (Secret.Secret [Text.Text] -> [App]) -> App
  Router :: forall a. Route.Route a -> (Secret.Secret a -> [App]) -> App
  Method :: forall env. (HTTP.StdMethod -> Bool) -> (env Natural.~> IO) -> Handler env -> App

match :: Text.Text -> [App] -> App
match = Match

literal :: (Web.ToHttpApiData a) => a -> [App] -> App
literal l = Match (Web.toUrlPiece l)

capture :: forall a. (Web.FromHttpApiData a, Typeable.Typeable a) => (Secret.Secret a -> [App]) -> App
capture = Capture

splat :: (Secret.Secret [Text.Text] -> [App]) -> App
splat = Splat

router :: forall a. Route.Route a -> (Secret.Secret a -> [App]) -> App
router = Router

method :: forall env. (HTTP.StdMethod -> Bool) -> (env Natural.~> IO) -> Handler env -> App
method = Method

get :: forall env. (env Natural.~> IO) -> Handler env -> App
get = Method (HTTP.GET ==)

getIO :: Handler IO -> App
getIO = get id

getPure :: Handler Identity.Identity -> App
getPure = get (return . Identity.runIdentity)

any :: forall env. (env Natural.~> IO) -> Handler env -> App
any = Method (const True)

release :: Wai.Request -> Secret.Secret a -> a
release req (Secret.Secret key) = case Vault.lookup key $ Wai.vault req of
  Nothing -> error "IMPOSSIBLE"
  Just val -> val

middleware :: App -> Wai.Middleware
middleware app backup req resp = do
  case app of
    Match text apps ->
      case Wai.pathInfo req of
        [] -> backup req resp
        (pathHead : pathTail) ->
          if pathHead == text
            then do
              let newReq = req {Wai.pathInfo = pathTail}
              middleware' apps backup newReq resp
            else backup req resp
    Capture @ty genApps ->
      case Wai.pathInfo req of
        [] -> backup req resp
        (pathHead : pathTail) ->
          case Web.parseUrlPiece @ty pathHead of
            Left _ -> backup req resp
            Right value -> do
              key <- Vault.newKey @ty
              let newVault = Vault.insert key value (Wai.vault req)
                  newReq = req {Wai.pathInfo = pathTail, Wai.vault = newVault}
              middleware' (genApps $ Secret.Secret key) backup newReq resp
    Splat genApps -> do
      key <- Vault.newKey @[Text.Text]
      let newVault = Vault.insert key (Wai.pathInfo req) (Wai.vault req)
          newReq = req {Wai.pathInfo = [], Wai.vault = newVault}
      middleware' (genApps $ Secret.Secret key) backup newReq resp
    Router @ty route genApps -> do
      case Route.exec route $ Wai.pathInfo req of
        (Left _, _) -> backup req resp
        (Right value, newPathInfo) -> do
          key <- Vault.newKey @ty
          let newVault = Vault.insert key value (Wai.vault req)
              newReq = req {Wai.pathInfo = newPathInfo, Wai.vault = newVault}
          middleware' (genApps $ Secret.Secret key) backup newReq resp
    Method pred trans handler ->
      case HTTP.parseMethod $ Wai.requestMethod req of
        Left _ -> backup req resp
        Right stdMethod ->
          if pred stdMethod
            then do
              res <- trans $ handler req
              resp res
            else backup req resp

middleware' :: [App] -> Wai.Middleware
middleware' [] backup req resp = backup req resp
middleware' (appsHead : appsTail) backup req resp =
  case appsHead of
    Match text apps ->
      case Wai.pathInfo req of
        [] -> middleware' appsTail backup req resp
        (pathHead : pathTail) ->
          if pathHead == text
            then do
              let newReq = req {Wai.pathInfo = pathTail}
              middleware' apps backup newReq resp
            else middleware' appsTail backup req resp
    Capture @ty genApps ->
      case Wai.pathInfo req of
        [] -> middleware' appsTail backup req resp
        (pathHead : pathTail) ->
          case Web.parseUrlPiece @ty pathHead of
            Left _ -> middleware' appsTail backup req resp
            Right value -> do
              key <- Vault.newKey @ty
              let newVault = Vault.insert key value (Wai.vault req)
                  newReq = req {Wai.pathInfo = pathTail, Wai.vault = newVault}
              middleware' (genApps $ Secret.Secret key) backup newReq resp
    Splat genApps -> do
      key <- Vault.newKey @[Text.Text]
      let newVault = Vault.insert key (Wai.pathInfo req) (Wai.vault req)
          newReq = req {Wai.pathInfo = [], Wai.vault = newVault}
      middleware' (genApps $ Secret.Secret key) backup newReq resp
    Router @ty route genApps -> do
      case Route.exec route $ Wai.pathInfo req of
        (Left _, _) -> middleware' appsTail backup req resp
        (Right value, newPathInfo) -> do
          key <- Vault.newKey @ty
          let newVault = Vault.insert key value (Wai.vault req)
              newReq = req {Wai.pathInfo = newPathInfo, Wai.vault = newVault}
          middleware' (genApps $ Secret.Secret key) backup newReq resp
    Method pred trans handler ->
      case Wai.pathInfo req of
        [] -> case HTTP.parseMethod $ Wai.requestMethod req of
          Left _ -> middleware' appsTail backup req resp
          Right stdMethod ->
            if pred stdMethod
              then do
                res <- trans $ handler req
                resp res
              else middleware' appsTail backup req resp
        _ -> middleware' appsTail backup req resp

tree :: App -> IO (Tree.Tree String)
tree (Match text apps) = do
  forest <- mapM tree apps
  return $ Tree.Node ("/" <> Text.unpack text) forest
tree (Capture @ty genApps) = do
  secret <- Secret.newSecret @ty
  forest <- mapM tree $ genApps secret
  return $ Tree.Node ("/:" <> showType @ty) forest
tree (Splat genApps) = do
  secret <- Secret.newSecret @[Text.Text]
  forest <- mapM tree $ genApps secret
  return $ Tree.Node "/*" forest
tree (Router @ty route genApps) = do
  secret <- Secret.newSecret @ty
  forest <- mapM tree $ genApps secret
  return $ Tree.Node (Text.unpack (Route.rep route)) forest
tree (Method pred _ _) = do
  return $ Tree.Node (List.intercalate " | " (map show $ filter pred [minBound ..])) []

showType :: forall a. (Typeable.Typeable a) => String
showType = show . Typeable.typeRep $ Typeable.Proxy @a

myApp =
  match
    "app"
    [ capture @Text.Text \text ->
        [ get id \req -> do
            undefined,
          capture @Int \age ->
            [ get id \req -> do
                let text' = release req text
                    age' = release req age
                undefined
            ]
        ],
      match
        "faq"
        [ get id \req -> do
            undefined,
          method (`elem` [HTTP.POST, HTTP.PUT, HTTP.PATCH]) id \req -> do
            undefined
        ]
    ]
