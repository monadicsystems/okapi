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
import qualified Text.InterpolatedString.Perl6 as Perl6

main :: IO ()
main = do
  -- Get database connection
  secret <- BS.readFile "secret.txt"
  let dbSettings = Hasql.settings "localhost" 5432 "realworld" secret "realworld"
      redisSettings = Redis.defaultConnectInfo
  Right dbConn <- Hasql.acquire dbSettings
  redisConn <- Redis.connect redisSettings

  -- Run server
  Okapi.run (App.execute $ App.AppEnv dbConn redisConn) $ conduitMiddleware conduitServer

conduitServer :: (Okapi.MonadServer m, Okapi.MonadSession m App.Session, App.Has Hasql.Connection App.AppEnv, App.Has Redis.Connection App.AppEnv) => m ()
conduitServer =
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

-- Home

home :: (Okapi.MonadServer m, Okapi.MonadSession m App.Session) => m ()
home = do
  Okapi.methodGET
  Okapi.pathEnd
  UI.writeLucid UI.Home

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
    _ -> do
      Okapi.write top
      handler
      Okapi.write bottom
  where
    top :: LBS.ByteString
    top = [Perl6.q|
    <!DOCTYPE html>
    <html>

    <head>
        <meta charset="utf-8">
        <title>Conduit</title>
        <link href="//code.ionicframework.com/ionicons/2.0.1/css/ionicons.min.css" rel="stylesheet" type="text/css">
        <link
            href="//fonts.googleapis.com/css?family=Titillium+Web:700|Source+Serif+Pro:400,700|Merriweather+Sans:400,700|Source+Sans+Pro:400,300,600,700,300italic,400italic,600italic,700italic"
            rel="stylesheet" type="text/css">
        <link rel="stylesheet" href="//demo.productionready.io/main.css">
        <script src="https://unpkg.com/htmx.org@1.8.0"></script>
    </head>

    <body>
        <nav id="navbar" class="navbar navbar-light">
            <div class="container"><a class="navbar-brand">conduit</a>
                <ul class="nav navbar-nav pull-xs-right">
                    <li class="nav-item"><a class="nav-link">Home</a></li>
                    <li class="nav-item"><a class="nav-link">Sign in</a></li>
                    <li class="nav-item"><a class="nav-link">Sign up</a></li>
                </ul>
            </div>
        </nav>
        <div id="content-slot">
    |]

    bottom :: LBS.ByteString
    bottom = [Perl6.q|
        </div>
        <footer>
            <div class="container"><a href="/" class="logo-font">conduit</a><span class="attribution"> An interactive
                    learning project from <a href="https://thinkster.io">Thinkster</a>. Code &amp; design licensed under
                    MIT. </span></div>
        </footer>
    </body>

    </html>
    |]
