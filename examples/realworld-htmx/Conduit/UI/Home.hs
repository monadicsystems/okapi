module Conduit.UI.Home where

import Lucid
  ( ToHtml (toHtml),
    a_,
    class_,
    div_,
    h1_,
    href_,
    id_,
    p_,
  )
import qualified Lucid

data Home = Home

instance Lucid.ToHtml Home where
  toHtml _ = do
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
  toHtmlRaw = toHtml
