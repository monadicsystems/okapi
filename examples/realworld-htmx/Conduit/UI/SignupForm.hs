{-# LANGUAGE InstanceSigs #-}

module Conduit.UI.SignupForm where

import qualified Conduit.Model.FormError as Model
import qualified Conduit.Model.SignupForm as Model
import Lucid
  ( a_,
    button_,
    class_,
    div_,
    fieldset_,
    form_,
    h1_,
    href_,
    input_,
    li_,
    name_,
    p_,
    placeholder_,
    type_,
    ul_,
    value_,
  )
import qualified Lucid
import qualified Lucid.Htmx as Htmx

data SignupForm = SignupForm (Maybe Model.SignupForm) [Model.FormError]

instance Lucid.ToHtml SignupForm where
  toHtml :: Monad m => SignupForm -> Lucid.HtmlT m ()
  toHtml (SignupForm maybeSignupFormData formErrors) = div_ [class_ "auth-page"] do
    div_ [class_ "container page"] do
      div_ [class_ "row"] do
        div_ [class_ "col-md-6 offset-md-3 col-xs-12"] do
          h1_ [class_ "text-xs-center"] "Sign up"
          p_ [class_ "text-xs-center"] $ a_ [Htmx.hxBoost_ "true", Htmx.hxTarget_ "#content-slot", Htmx.hxPushUrl_ "true", href_ "/form/signin"] "Have an account?"
          case formErrors of
            [] -> ""
            formErrors' -> ul_ [class_ "error-messages"] $ mapM_ (li_ [] . Lucid.toHtml) formErrors'
          form_ [Htmx.hxPost_ "/signup", Htmx.hxTarget_ "#content-slot"] do
            fieldset_ [class_ "form-group"] do
              input_
                [ class_ "form-control form-control-lg",
                  type_ "text",
                  name_ "username",
                  placeholder_ "Username",
                  value_ $ maybe "" Model.username maybeSignupFormData
                ]
            fieldset_ [class_ "form-group"] do
              input_
                [ class_ "form-control form-control-lg",
                  type_ "text",
                  name_ "email",
                  placeholder_ "Email",
                  value_ $ maybe "" Model.email maybeSignupFormData
                ]
            fieldset_ [class_ "form-group"] do
              input_
                [ class_ "form-control form-control-lg",
                  type_ "password",
                  name_ "password",
                  placeholder_ "Password",
                  value_ $ maybe "" Model.password maybeSignupFormData
                ]
            button_ [class_ "btn btn-lg btn-primary pull-xs-right"] "Sign up"
  toHtmlRaw :: Monad m => SignupForm -> Lucid.HtmlT m ()
  toHtmlRaw = Lucid.toHtml
