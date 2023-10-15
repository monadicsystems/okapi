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

module Tree where

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
import Web.HttpApiData qualified as Web

newtype Secret a = Secret (Vault.Key a)

type Handler env = Wai.Request -> env Wai.Response

data App where
  Match :: Text.Text -> [App] -> App
  Capture :: forall a. (Web.FromHttpApiData a, Typeable.Typeable a) => (Secret a -> [App]) -> App
  Splat :: (Secret [Text.Text] -> [App]) -> App
  Router :: forall a. Route.Route a -> (Secret a -> [App]) -> App
  Method :: forall env. (HTTP.StdMethod -> Bool) -> (env Natural.~> IO) -> Handler env -> App

match :: Text.Text -> [App] -> App
match = Match

(/?) = match

literal :: (Web.ToHttpApiData a) => a -> [App] -> App
literal l = Match (Web.toUrlPiece l)

(/==) :: (Web.ToHttpApiData a) => a -> [App] -> App
(/==) = literal

capture :: forall a. (Web.FromHttpApiData a, Typeable.Typeable a) => (Secret a -> [App]) -> App
capture = Capture

(/:) :: forall a. (Web.FromHttpApiData a, Typeable.Typeable a) => (Secret a -> [App]) -> App
(/:) = capture

splat :: (Secret [Text.Text] -> [App]) -> App
splat = Splat

router :: forall a. Route.Route a -> (Secret a -> [App]) -> App
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

myApp' =
  "app"
    /? [ (/:) \(text :: Secret Text.Text) ->
           [ get id \req -> do
               undefined,
             capture @Int \age ->
               [ get id \req -> do
                   let text' = release req text
                       age' = release req age
                   undefined
               ]
           ],
         (5 :: Int)
           /== [ getPure \req -> do
                   undefined
               , splat \args ->
                   [ method (HTTP.HEAD==) id \req -> undefined
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

release :: Wai.Request -> Secret a -> a
release req (Secret key) = case Vault.lookup key $ Wai.vault req of
  Nothing -> error "IMPOSSIBLE"
  Just val -> val

toWaiApplication :: App -> Wai.Middleware
toWaiApplication app backup req resp = do
  case app of
    Match text apps ->
      case Wai.pathInfo req of
        [] -> backup req resp
        (pathHead : pathTail) ->
          if pathHead == text
            then do
              let newReq = req {Wai.pathInfo = pathTail}
              toWaiApplication' apps backup newReq resp
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
              toWaiApplication' (genApps $ Secret key) backup newReq resp
    Splat genApps -> do
      key <- Vault.newKey @[Text.Text]
      let newVault = Vault.insert key (Wai.pathInfo req) (Wai.vault req)
          newReq = req {Wai.pathInfo = [], Wai.vault = newVault}
      toWaiApplication' (genApps $ Secret key) backup newReq resp
    Router @ty route genApps -> do
      case Route.exec route $ Wai.pathInfo req of
        (Left _, _) -> backup req resp
        (Right value, newPathInfo) -> do
          key <- Vault.newKey @ty
          let newVault = Vault.insert key value (Wai.vault req)
              newReq = req {Wai.pathInfo = newPathInfo, Wai.vault = newVault}
          toWaiApplication' (genApps $ Secret key) backup newReq resp
    Method pred trans handler ->
      case HTTP.parseMethod $ Wai.requestMethod req of
        Left _ -> backup req resp
        Right stdMethod ->
          if pred stdMethod
            then do
              res <- trans $ handler req
              resp res
            else backup req resp

toWaiApplication' :: [App] -> Wai.Middleware
toWaiApplication' [] backup req resp = backup req resp
toWaiApplication' (appsHead : appsTail) backup req resp =
  case appsHead of
    Match text apps ->
      case Wai.pathInfo req of
        [] -> toWaiApplication' appsTail backup req resp
        (pathHead : pathTail) ->
          if pathHead == text
            then do
              let newReq = req {Wai.pathInfo = pathTail}
              toWaiApplication' apps backup newReq resp
            else toWaiApplication' appsTail backup req resp
    Capture @ty genApps ->
      case Wai.pathInfo req of
        [] -> toWaiApplication' appsTail backup req resp
        (pathHead : pathTail) ->
          case Web.parseUrlPiece @ty pathHead of
            Left _ -> toWaiApplication' appsTail backup req resp
            Right value -> do
              key <- Vault.newKey @ty
              let newVault = Vault.insert key value (Wai.vault req)
                  newReq = req {Wai.pathInfo = pathTail, Wai.vault = newVault}
              toWaiApplication' (genApps $ Secret key) backup newReq resp
    Splat genApps -> do
      key <- Vault.newKey @[Text.Text]
      let newVault = Vault.insert key (Wai.pathInfo req) (Wai.vault req)
          newReq = req {Wai.pathInfo = [], Wai.vault = newVault}
      toWaiApplication' (genApps $ Secret key) backup newReq resp
    Router @ty route genApps -> do
      case Route.exec route $ Wai.pathInfo req of
        (Left _, _) -> toWaiApplication' appsTail backup req resp
        (Right value, newPathInfo) -> do
          key <- Vault.newKey @ty
          let newVault = Vault.insert key value (Wai.vault req)
              newReq = req {Wai.pathInfo = newPathInfo, Wai.vault = newVault}
          toWaiApplication' (genApps $ Secret key) backup newReq resp
    Method pred trans handler ->
      case Wai.pathInfo req of
        [] -> case HTTP.parseMethod $ Wai.requestMethod req of
          Left _ -> toWaiApplication' appsTail backup req resp
          Right stdMethod ->
            if pred stdMethod
              then do
                res <- trans $ handler req
                resp res
              else toWaiApplication' appsTail backup req resp
        _ -> toWaiApplication' appsTail backup req resp

toTree :: App -> IO (Tree.Tree String)
toTree (Match text apps) = do
  forest <- mapM toTree apps
  return $ Tree.Node ("/" <> Text.unpack text) forest
toTree (Capture @ty genApps) = do
  secret <- newSecret @ty
  forest <- mapM toTree $ genApps secret
  return $ Tree.Node ("/{:" <> showType @ty <> "}") forest
toTree (Splat genApps) = do
  secret <- newSecret @[Text.Text]
  forest <- mapM toTree $ genApps secret
  return $ Tree.Node "/*" forest
toTree (Router @ty route genApps) = do
  secret <- newSecret @ty
  forest <- mapM toTree $ genApps secret
  return $ Tree.Node (Text.unpack (Route.rep route)) forest
toTree (Method pred _ _) = do
  return $ Tree.Node (List.intercalate " | " (map show $ filter pred [minBound ..])) []

showType :: forall a. (Typeable.Typeable a) => String
showType = show . Typeable.typeRep $ Typeable.Proxy @a

newSecret :: forall a. IO (Secret a)
newSecret = Secret <$> Vault.newKey

{-
data App where
  Match :: Text.Text -> [App] -> App
  Capture :: forall a. (Web.FromHttpApiData a, Typeable.Typeable a) => (Secret a -> [App]) -> App
  Splat :: (Secret [Text.Text] -> [App]) -> App
  Router :: forall a. Route.Route a -> (Secret a -> [App]) -> App
  Method :: forall env. (HTTP.StdMethod -> Bool) -> (env Natural.~> IO) -> Handler env -> App
-}
