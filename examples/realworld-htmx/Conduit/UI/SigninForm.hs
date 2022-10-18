{-# LANGUAGE InstanceSigs #-}

module Conduit.UI.SigninForm where

import qualified Conduit.Model.FormError as Model
import qualified Conduit.Model.SigninForm as Model
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

data SigninForm = SigninForm (Maybe Model.SigninForm) [Model.FormError]

instance Lucid.ToHtml SigninForm where
  toHtml :: Monad m => SigninForm -> Lucid.HtmlT m ()
  toHtml (SigninForm maybeSigninFormData formErrors) = div_ [class_ "auth-page"] do
    div_ [class_ "container page"] do
      div_ [class_ "row"] do
        div_ [class_ "col-md-6 offset-md-3 col-xs-12"] do
          h1_ [class_ "text-xs-center"] "Sign in"
          p_ [class_ "text-xs-center"] $ a_ [Htmx.hxBoost_ "true", Htmx.hxTarget_ "#content-slot", Htmx.hxPushUrl_ "true", href_ "/form/signup"] "Need an account?"
          case formErrors of
            [] -> ""
            formErrors' -> ul_ [class_ "error-messages"] $ mapM_ (li_ [] . Lucid.toHtml) formErrors'
          form_ [Htmx.hxPost_ "/login", Htmx.hxTarget_ "#content-slot"] do
            fieldset_ [class_ "form-group"] do
              input_
                [ class_ "form-control form-control-lg",
                  type_ "text",
                  name_ "email",
                  placeholder_ "Email",
                  value_ $ maybe "" Model.email maybeSigninFormData
                ]
            fieldset_ [class_ "form-group"] do
              input_
                [ class_ "form-control form-control-lg",
                  type_ "password",
                  name_ "password",
                  placeholder_ "Password",
                  value_ $ maybe "" Model.password maybeSigninFormData
                ]
            button_ [class_ "btn btn-lg btn-primary pull-xs-right"] "Sign in"
  toHtmlRaw :: Monad m => SigninForm -> Lucid.HtmlT m ()
  toHtmlRaw = Lucid.toHtml
