{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import qualified Control.Monad as Monad
import qualified Control.Monad.Reader as Reader
import qualified Control.Monad.IO.Class as IO
import qualified Control.Monad.Combinators as Combinators
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Function ((&))
import qualified Data.Text as Text
import qualified Data.Word as Word
import qualified Database.Redis as Redis
import qualified GHC.Generics as Generics
import qualified Hasql.Connection as Hasql
import qualified Lucid
import Lucid
    ( h1_
    , head_
    , html_
    , meta_
    , src_
    , charset_
    , title_
    , link_
    , href_
    , rel_
    , type_
    , script_
    , li_
    , ul_
    , body_
    , footer_
    , a_
    , div_
    , class_
    , span_
    , id_
    , nav_
    , p_
    )
import qualified Lucid.Htmx as Htmx
import qualified Okapi
import qualified Rel8
import qualified System.Random as Random

newtype App a = App
    { unApp :: Reader.ReaderT AppEnv IO a
    }
    deriving newtype (Functor, Applicative, Monad, IO.MonadIO, Reader.MonadReader AppEnv)

data AppEnv = AppEnv
  { appEnvDBConnection :: Hasql.Connection
  , appEnvRedisConnection :: Redis.Connection
  }

class Has field env where
    obtain :: env -> field

instance Has Hasql.Connection AppEnv where
    obtain = appEnvDBConnection

instance Has Redis.Connection AppEnv where
    obtain = appEnvRedisConnection

grab :: forall field env m. (Reader.MonadReader env m, Has field env) => m field
grab = Reader.asks $ obtain @field

data Session = Session
  { sessionUsername :: Text.Text
  , sessionEmail :: Text.Text
  } deriving (Eq, Show, Generics.Generic, Aeson.ToJSON, Aeson.FromJSON)

instance Okapi.MonadSession App Session where
  sessionSecret = IO.liftIO $ BS.readFile "secret.txt"
  generateSessionID = IO.liftIO $ do
    words :: [Word.Word8] <- Monad.replicateM 20 Random.randomIO
    pure $ Okapi.SessionID $ BS.pack words
  getSession (Okapi.SessionID sessionID) = do
    redisConnection <- grab @Redis.Connection
    redisResult <- IO.liftIO $ Redis.runRedis redisConnection do
      eitherMbValueBS <- Redis.get sessionID
      pure $ case eitherMbValueBS of
        Left _ -> Nothing
        Right mbValueBS -> case mbValueBS of
          Nothing -> Nothing
          Just valueBS -> Aeson.decodeStrict valueBS
    IO.liftIO $ Redis.disconnect redisConnection
    pure redisResult
  putSession (Okapi.SessionID sessionID) session = do
    redisConnection <- grab @Redis.Connection
    _ <- IO.liftIO $ Redis.runRedis redisConnection $
      Redis.set sessionID (LBS.toStrict $ Aeson.encode session)
    IO.liftIO $ Redis.disconnect redisConnection
  clearSession (Okapi.SessionID sessionID) = do
    redisConnection <- grab @Redis.Connection
    _ <- IO.liftIO $ Redis.runRedis redisConnection $ Redis.del [sessionID]
    IO.liftIO $ Redis.disconnect redisConnection

main :: IO ()
main = do
    -- Get database connection
    secret <- BS.readFile "secret.txt"
    let
      dbSettings = Hasql.settings "localhost" 5432 "realworld" secret "realworld"
      redisSettings = Redis.defaultConnectInfo
    Right dbConn <- Hasql.acquire dbSettings
    redisConn <- Redis.connect redisSettings 

    -- Run server
    Okapi.run (executeApp $ AppEnv dbConn redisConn) $ Okapi.withSession server

executeApp :: AppEnv -> App a -> IO a
executeApp appEnv (App app) = Reader.runReaderT app appEnv

setLucid :: Lucid.Html () -> Okapi.Response -> Okapi.Response
setLucid = Okapi.setHTML . Lucid.renderBS

server :: (Okapi.MonadOkapi m, Has Hasql.Connection AppEnv, Has Redis.Connection AppEnv) => m Okapi.Response
server = Combinators.choice
    [ home
    , signupForm
    , submitSignupForm
    , loginForm
    , submitLoginForm
    , logout
    , follow
    , unfollow
    ]

-- Home

home :: Okapi.MonadOkapi m => m Okapi.Response
home = homeRoute >> homeHandler

homeRoute :: Okapi.MonadOkapi m => m ()
homeRoute = do
  Okapi.methodGET
  Okapi.pathEnd

homeHandler :: Monad m => m Okapi.Response
homeHandler =
    Okapi.ok
    & setLucid do
        wrapHtml do -- TODO: Only wrap if not htmx request
          div_ [class_ "home-page"] do
            div_ [class_ "banner"] do
              div_ [class_ "container"] do
                h1_ [class_ "logo-font"] "conduit"
                p_ "A place to share your knowledge."
            div_ [class_ "container page"] do
              div_ [class_ "row"] do
                -- FEED
                div_ [id_ "feeds", class_ "col-md-9"] do
                    h1_ [] "Empty Feed"
                -- TAGS
                div_ [id_ "tags", class_ "col-md-3"] do
                  div_ [class_ "sidebar"] $ do
                    p_ "Popular Tags"
                    div_ [class_ "tag-list"] do
                      a_ [href_ "#", class_ "tag-pill tag-default"] "fake-tag"
    & return

emptyHtml :: Lucid.Html ()
emptyHtml = ""

wrapHtml :: Lucid.Html () -> Lucid.Html ()
wrapHtml innerHtml =
    html_ do
      head_ do
        meta_ [charset_ "utf-8"]
        title_ "Conduit"
        link_ [href_ "//code.ionicframework.com/ionicons/2.0.1/css/ionicons.min.css", rel_ "stylesheet", type_ "text/css"]
        link_ [href_ "//fonts.googleapis.com/css?family=Titillium+Web:700|Source+Serif+Pro:400,700|Merriweather+Sans:400,700|Source+Sans+Pro:400,300,600,700,300italic,400italic,600italic,700italic", rel_ "stylesheet", type_ "text/css"]
        link_ [rel_ "stylesheet", href_ "//demo.productionready.io/main.css"]
        script_ [src_ "https://unpkg.com/htmx.org@1.8.0"] emptyHtml
      body_ do
        -- Navbar
        nav_ [ id_ "navbar", class_ "navbar navbar-light"] do
          div_ [class_ "container"] do
            -- TODO: Does it make sense to use hxGet_ and href_ for boost?
            a_ [class_ "navbar-brand"] "conduit"
            ul_ [class_ "nav navbar-nav pull-xs-right"] do
              li_ [class_ "nav-item"] do
                a_ [class_ "nav-link"] "Home"
              li_ [class_ "nav-item"] do
                a_ [class_ "nav-link"] "Sign in"
              li_ [class_ "nav-item"] do
                a_ [class_ "nav-link"] "Sign up"

        -- Content
        div_ [id_ "content-slot"] innerHtml

        -- Footer
        footer_ do
          div_ [class_ "container"] do
            a_ [href_ "/", class_ "logo-font"] "conduit"
            span_ [class_ "attribution"] do
              "          An interactive learning project from "
              a_ [href_ "https://thinkster.io"] "Thinkster"
              ". Code & design licensed under MIT.        "

-- Signup Form

signupForm :: (Okapi.MonadOkapi m) => m Okapi.Response
signupForm = signupFormRoute >>= signupFormHandler

signupFormRoute :: Okapi.MonadOkapi m => m ()
signupFormRoute = undefined

signupFormHandler :: () -> m Okapi.Response
signupFormHandler = undefined

-- Submit Signup Form

submitSignupForm :: (Okapi.MonadOkapi m) => m Okapi.Response
submitSignupForm = submitSignupFormRoute >>= submitSignupFormHandler

submitSignupFormRoute :: Okapi.MonadOkapi m => m ()
submitSignupFormRoute = undefined

submitSignupFormHandler :: () -> m Okapi.Response
submitSignupFormHandler = undefined

-- Login Form

loginForm :: Okapi.MonadOkapi m => m Okapi.Response
loginForm = loginFormRoute >>= loginFormHandler

loginFormRoute :: Okapi.MonadOkapi m => m ()
loginFormRoute = undefined

loginFormHandler :: () -> m Okapi.Response
loginFormHandler = undefined

-- Submit Login Form

submitLoginForm :: Okapi.MonadOkapi m => m Okapi.Response
submitLoginForm = submitLoginFormRoute >>= submitLoginFormHandler

submitLoginFormRoute :: Okapi.MonadOkapi m => m ()
submitLoginFormRoute = undefined

submitLoginFormHandler :: () -> m Okapi.Response
submitLoginFormHandler = undefined

-- Logout

logout :: Okapi.MonadOkapi m => m Okapi.Response
logout = logoutRoute >>= logoutHandler

logoutRoute :: Okapi.MonadOkapi m => m ()
logoutRoute = undefined

logoutHandler :: () -> m Okapi.Response
logoutHandler = undefined

-- Follow

follow :: Okapi.MonadOkapi m => m Okapi.Response
follow = followRoute >>= followHandler

followRoute :: Okapi.MonadOkapi m => m ()
followRoute = undefined

followHandler :: () -> m Okapi.Response
followHandler = undefined

-- Unfollow

unfollow :: Okapi.MonadOkapi m => m Okapi.Response
unfollow = unfollowRoute >>= unfollowHandler

unfollowRoute :: Okapi.MonadOkapi m => m ()
unfollowRoute = undefined

unfollowHandler :: () -> m Okapi.Response
unfollowHandler = undefined
