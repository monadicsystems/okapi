module Main where

import qualified Conduit.App as App
import qualified Conduit.UI as UI
import qualified Control.Monad as Monad
import qualified Control.Monad.Combinators as Combinators
import qualified Control.Monad.IO.Class as IO
import qualified Control.Monad.Reader as Reader
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Function ((&))
import qualified Data.Text as Text
import qualified Data.Word as Word
import qualified Database.Redis as Redis
import qualified GHC.Generics as Generics
import qualified Hasql.Connection as Hasql
import Lucid
  ( a_,
    body_,
    charset_,
    class_,
    div_,
    footer_,
    h1_,
    head_,
    href_,
    html_,
    id_,
    li_,
    link_,
    meta_,
    nav_,
    p_,
    rel_,
    script_,
    span_,
    src_,
    title_,
    type_,
    ul_,
  )
import qualified Lucid
import qualified Lucid.Htmx as Htmx
import Network.Wai (Middleware)
import qualified Okapi
import qualified Rel8
import qualified System.Random as Random

main :: IO ()
main = do
  -- Get database connection
  secret <- BS.readFile "secret.txt"
  let dbSettings = Hasql.settings "localhost" 5432 "realworld" secret "realworld"
      redisSettings = Redis.defaultConnectInfo
  Right dbConn <- Hasql.acquire dbSettings
  redisConn <- Redis.connect redisSettings

  -- Run server
  Okapi.run (App.execute $ App.AppEnv dbConn redisConn) conduitServer

conduitServer :: (Okapi.MonadServer m, Okapi.MonadSession m App.Session, App.Has Hasql.Connection App.AppEnv, App.Has Redis.Connection App.AppEnv) => m ()
conduitServer = home

{-
  Combinators.choice
    [ home,
      signupForm,
      submitSignupForm,
      loginForm,
      submitLoginForm,
      logout,
      follow,
      unfollow
    ]
-}

-- Home

home :: (Okapi.MonadServer m, Okapi.MonadSession m App.Session) => m ()
home = do
  Okapi.methodGET
  Okapi.pathEnd
  UI.writeLucid $ UI.Wrapper UI.Home

-- Signup Form

signupForm :: Okapi.MonadServer m => m ()
signupForm = signupFormRoute >>= signupFormHandler

signupFormRoute :: Okapi.MonadServer m => m ()
signupFormRoute = undefined

signupFormHandler :: () -> m ()
signupFormHandler = undefined

-- Submit Signup Form

submitSignupForm :: Okapi.MonadServer m => m ()
submitSignupForm = submitSignupFormRoute >>= submitSignupFormHandler

submitSignupFormRoute :: Okapi.MonadServer m => m ()
submitSignupFormRoute = undefined

submitSignupFormHandler :: () -> m ()
submitSignupFormHandler = undefined

-- Login Form

loginForm :: Okapi.MonadServer m => m ()
loginForm = loginFormRoute >>= loginFormHandler

loginFormRoute :: Okapi.MonadServer m => m ()
loginFormRoute = undefined

loginFormHandler :: () -> m ()
loginFormHandler = undefined

-- Submit Login Form

submitLoginForm :: Okapi.MonadServer m => m ()
submitLoginForm = submitLoginFormRoute >>= submitLoginFormHandler

submitLoginFormRoute :: Okapi.MonadServer m => m ()
submitLoginFormRoute = undefined

submitLoginFormHandler :: () -> m ()
submitLoginFormHandler = undefined

-- Logout

logout :: Okapi.MonadServer m => m ()
logout = logoutRoute >>= logoutHandler

logoutRoute :: Okapi.MonadServer m => m ()
logoutRoute = undefined

logoutHandler :: () -> m ()
logoutHandler = undefined

-- Follow

follow :: Okapi.MonadServer m => m ()
follow = followRoute >>= followHandler

followRoute :: Okapi.MonadServer m => m ()
followRoute = undefined

followHandler :: () -> m ()
followHandler = undefined

-- Unfollow

unfollow :: Okapi.MonadServer m => m ()
unfollow = unfollowRoute >>= unfollowHandler

unfollowRoute :: Okapi.MonadServer m => m ()
unfollowRoute = undefined

unfollowHandler :: () -> m ()
unfollowHandler = undefined

conduitMiddleware ::
  ( Okapi.MonadServer m,
    Okapi.MonadSession m App.Session
  ) =>
  Okapi.Middleware m
conduitMiddleware = wrapIfNotHtmxRequest . Okapi.withSession

wrapIfNotHtmxRequest :: Okapi.MonadServer m => Okapi.Middleware m
wrapIfNotHtmxRequest handler = do
  hxRequestHeaderValue <- Combinators.optional $ Okapi.header "HX-Request"
  case hxRequestHeaderValue of
    Just "true" -> handler
    _ -> handler
