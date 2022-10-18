{-# LANGUAGE InstanceSigs #-}

module Conduit.UI
  ( Home (..),
    emptyHtml,
    writeLucid
  )
where

import Conduit.UI.Home (Home (..))
import qualified Data.ByteString.Lazy as LBS
import Lucid
  ( a_,
    body_,
    charset_,
    class_,
    div_,
    footer_,
    head_,
    href_,
    html_,
    id_,
    li_,
    link_,
    meta_,
    nav_,
    rel_,
    renderBS,
    script_,
    span_,
    src_,
    title_,
    type_,
    ul_,
  )
import qualified Lucid
import qualified Okapi

instance Okapi.Writeable (Lucid.Html ()) where
  toLBS :: Lucid.Html () -> LBS.ByteString
  toLBS = renderBS

emptyHtml :: Lucid.Html ()
emptyHtml = ""

-- newtype Wrapper a = Wrapper a

{-
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
        nav_ [id_ "navbar", class_ "navbar navbar-light"] do
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
-}

-- instance Lucid.ToHtml a => Lucid.ToHtml (Wrapper a) where
--   toHtml (Wrapper innerHtml) =
--     html_ do
--       head_ do
--         meta_ [charset_ "utf-8"]
--         title_ "Conduit"
--         link_ [href_ "//code.ionicframework.com/ionicons/2.0.1/css/ionicons.min.css", rel_ "stylesheet", type_ "text/css"]
--         link_ [href_ "//fonts.googleapis.com/css?family=Titillium+Web:700|Source+Serif+Pro:400,700|Merriweather+Sans:400,700|Source+Sans+Pro:400,300,600,700,300italic,400italic,600italic,700italic", rel_ "stylesheet", type_ "text/css"]
--         link_ [rel_ "stylesheet", href_ "//demo.productionready.io/main.css"]
--         script_ [src_ "https://unpkg.com/htmx.org@1.8.0"] emptyHtml
--       body_ do
--         -- Navbar
--         nav_ [id_ "navbar", class_ "navbar navbar-light"] do
--           div_ [class_ "container"] do
--             -- TODO: Does it make sense to use hxGet_ and href_ for boost?
--             a_ [class_ "navbar-brand"] "conduit"
--             ul_ [class_ "nav navbar-nav pull-xs-right"] do
--               li_ [class_ "nav-item"] do
--                 a_ [class_ "nav-link"] "Home"
--               li_ [class_ "nav-item"] do
--                 a_ [class_ "nav-link"] "Sign in"
--               li_ [class_ "nav-item"] do
--                 a_ [class_ "nav-link"] "Sign up"

--         -- Content
--         div_ [id_ "content-slot"] $ Lucid.toHtml innerHtml

--         -- Footer
--         footer_ do
--           div_ [class_ "container"] do
--             a_ [href_ "/", class_ "logo-font"] "conduit"
--             span_ [class_ "attribution"] do
--               "          An interactive learning project from "
--               a_ [href_ "https://thinkster.io"] "Thinkster"
--               ". Code & design licensed under MIT.        "
--   toHtmlRaw = Lucid.toHtml

writeLucid :: (Okapi.MonadServer m, Lucid.ToHtml a) => a -> m ()
writeLucid htmlable = do
  Okapi.setHeader ("Content-Type", "text/html;charset=utf-8")
  Okapi.write $ toHtmlPure htmlable
  where
    toHtmlPure :: Lucid.ToHtml a => a -> Lucid.Html ()
    toHtmlPure = Lucid.toHtml
