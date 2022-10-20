module Main where

import qualified Data.Maybe as Maybe
import qualified Control.Monad.IO.Class as IO
import qualified Conduit.App as App
import qualified Conduit.Model.FormError as Model
import qualified Conduit.Model.SigninForm as Model
import qualified Conduit.Model.SignupForm as Model
import qualified Conduit.Model.User as Model
import qualified Conduit.UI as UI
import qualified Conduit.UI.SigninForm as UI
import qualified Conduit.UI.SignupForm as UI
import qualified Conduit.Validate as Validate
import qualified Control.Monad as Monad
import qualified Control.Monad.Combinators as Combinators
import qualified Control.Monad.IO.Class as IO
import qualified Control.Monad.Reader as Reader
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Function ((&))
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Data.Word as Word
import qualified Database.Redis as Redis
import qualified GHC.Generics as Generics
import qualified Hasql.Connection as Hasql.Connection
import qualified Conduit.Database.User as Database
import qualified Conduit.Database as Database
import qualified Conduit.Model.Newtype as Model
import qualified Hasql.Connection as Hasql (settings)
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
import qualified Okapi
import qualified Rel8
import qualified System.Random as Random
import qualified Text.InterpolatedString.Perl6 as Perl6
import qualified Web.Forma as Forma
-- import qualified Hasql.Connection as Hasql
import qualified Hasql.Pool as Hasql

main :: IO ()
main = do
  -- Get database connection
  secret <- BS.readFile "secret.txt"
  let dbSettings = Hasql.settings "localhost" 5432 "realworld" secret "realworld"
      redisSettings = Redis.defaultConnectInfo
  dbPool <- Hasql.acquire 100 (Just 10) dbSettings
  redisConn <- Redis.connect redisSettings

  -- Run server
  Okapi.run (App.execute $ App.AppEnv dbPool redisConn) $ conduitMiddleware conduitServer

conduitServer :: (Okapi.MonadServer m, Okapi.MonadSession m Model.User, Reader.MonadReader App.AppEnv m, IO.MonadIO m) => m ()
conduitServer =
  Combinators.choice
    [ home,
      signupForm,
      submitSignupForm,
      signinForm,
      submitSigninForm
      -- logout,
      -- follow,
      -- unfollow
    ]

-- Home

home :: (Okapi.MonadServer m, Okapi.MonadSession m Model.User) => m ()
home = homeRoute >>= homeHandler

homeRoute :: Okapi.MonadServer m => m ()
homeRoute = do
  Okapi.methodGET
  Okapi.pathEnd

homeHandler :: (Okapi.MonadServer m, Okapi.MonadSession m Model.User) => () -> m ()
homeHandler _ = UI.writeLucid UI.Home

-- Signup Form

signupForm :: Okapi.MonadServer m => m ()
signupForm = signupFormRoute >>= signupFormHandler
  where
    signupFormRoute :: Okapi.MonadServer m => m ()
    signupFormRoute = do
      Okapi.methodGET
      Okapi.pathParam @Text.Text `Okapi.is` "form"
      Okapi.pathParam @Text.Text `Okapi.is` "signup"
      Okapi.pathEnd

    signupFormHandler :: Okapi.MonadServer m => () -> m ()
    signupFormHandler _ = UI.writeLucid $ UI.SignupForm Nothing []

-- Submit Signup Form

submitSignupForm :: (Okapi.MonadServer m, Okapi.MonadSession m Model.User, Reader.MonadReader App.AppEnv m, IO.MonadIO m) => m ()
submitSignupForm = submitSignupFormRoute >>= submitSignupFormHandler
  where
    submitSignupFormRoute :: Okapi.MonadServer m => m ()
    submitSignupFormRoute = do
      Okapi.methodPOST
      Okapi.pathParam @Text.Text `Okapi.is` "signup"
      Okapi.pathEnd

    submitSignupFormHandler :: (Okapi.MonadServer m, Okapi.MonadSession m Model.User, Reader.MonadReader App.AppEnv m, IO.MonadIO m) => () -> m ()
    submitSignupFormHandler _ = do
      signupForm <- Okapi.bodyURLEncoded @Model.SignupForm
      let formResult = Validate.signupForm signupForm
      case formResult of
        Forma.ParsingFailed _ formParserError -> do
          UI.writeLucid $ UI.SignupForm (Just signupForm) [Model.FormError formParserError]
        Forma.ValidationFailed formErrorMap -> do
          let formErrors =
                Prelude.map (\(field, f) -> Model.FormError $ f $ Forma.showFieldName field) $
                  Map.toList formErrorMap
          UI.writeLucid $ UI.SignupForm (Just signupForm) formErrors
        Forma.Succeeded validatedSignupForm -> do
          -- TODO: Write result to DB and store session
          -- generate salt
          -- hashpassword
          -- bam
          maybeNewUserKey <- Database.runStatementOne $ Rel8.insert $ Database.insertNewUser validatedSignupForm (Model.Salt "", Model.HashedPassword "123")
          case maybeNewUserKey of
            Nothing -> Okapi.throw Okapi.internalServerError
            Just newUserKey -> do
              maybeNewUserRow <- Database.runStatementOne $ Rel8.select $ Database.queryUserByKey newUserKey
              case maybeNewUserRow of
                Nothing -> Okapi.throw Okapi.internalServerError
                Just newUserRow -> Okapi.createSession $ Database.userRowToUser newUserRow
              Okapi.redirect 301 "/"

-- Signin Form

signinForm :: Okapi.MonadServer m => m ()
signinForm = signinFormRoute >>= signinFormHandler
  where
    signinFormRoute :: Okapi.MonadServer m => m ()
    signinFormRoute = do
      Okapi.methodGET
      Okapi.pathParam @Text.Text `Okapi.is` "form"
      Okapi.pathParam @Text.Text `Okapi.is` "signin"
      Okapi.pathEnd

    signinFormHandler :: Okapi.MonadServer m => () -> m ()
    signinFormHandler _ = UI.writeLucid $ UI.SigninForm Nothing []

-- Submit Signin Form

submitSigninForm :: Okapi.MonadServer m => m ()
submitSigninForm = submitSigninFormRoute >>= submitSigninFormHandler
  where
    submitSigninFormRoute :: Okapi.MonadServer m => m ()
    submitSigninFormRoute = do
      Okapi.methodPOST
      Okapi.pathParam @Text.Text `Okapi.is` "signin"
      Okapi.pathEnd

    submitSigninFormHandler :: Okapi.MonadServer m => () -> m ()
    submitSigninFormHandler _ = do
      signinForm <- Okapi.bodyURLEncoded @Model.SigninForm
      let formResult = Validate.signinForm signinForm
      case formResult of
        Forma.ParsingFailed _ formParserError -> do
          UI.writeLucid $ UI.SigninForm (Just signinForm) [Model.FormError formParserError]
        Forma.ValidationFailed formErrorMap -> do
          let formErrors =
                Prelude.map (\(field, f) -> Model.FormError $ f $ Forma.showFieldName field) $
                  Map.toList formErrorMap
          UI.writeLucid $ UI.SigninForm (Just signinForm) formErrors
        Forma.Succeeded validatedSignupForm -> do
          -- TODO: Write result to DB and store session
          Okapi.redirect 301 "/"

-- Logout

logout :: Okapi.MonadServer m => m ()
logout = do
  Okapi.methodPOST
  Okapi.pathParam @Text.Text `Okapi.is` "logout"
  Okapi.pathEnd
  Okapi.redirect 301 "/"

-- Follow

follow :: Okapi.MonadServer m => m ()
follow = undefined

-- Unfollow

unfollow :: Okapi.MonadServer m => m ()
unfollow = undefined

conduitMiddleware ::
  ( Okapi.MonadServer m,
    Okapi.MonadSession m Model.User
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
    top =
      [Perl6.q|
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
        <nav id="navbar" class="navbar navbar-light" hx-boost="true" hx-target="#content-slot" hx-push-url="true">
            <div class="container"><a class="navbar-brand" hx-get="/" href="/">conduit</a>
                <ul class="nav navbar-nav pull-xs-right">
                    <li class="nav-item"><a class="nav-link" hx-get="/" href="/">Home</a></li>
                    <li class="nav-item"><a class="nav-link" hx-get="/form/signin" href="/form/sigin">Sign in</a></li>
                    <li class="nav-item"><a class="nav-link" hx-get="/form/signup" href="/form/signup">Sign up</a></li>
                </ul>
            </div>
        </nav>
        <div id="content-slot">
    |]

    bottom :: LBS.ByteString
    bottom =
      [Perl6.q|
        </div>
        <footer>
            <div hx-boost="true" hx-push-url="true" hx-target="#content-slot" class="container"><a hx-get="/" href="/" class="logo-font">conduit</a><span class="attribution"> An interactive
                    learning project from <a href="https://thinkster.io">Thinkster</a>. Code &amp; design licensed under
                    MIT. </span></div>
        </footer>
    </body>

    </html>
    |]
