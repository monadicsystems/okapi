{-# LANGUAGE InstanceSigs #-}

module Conduit.UI
  ( Home (..),
    emptyHtml,
    writeLucid
  )
where

import Conduit.UI.Home (Home (..))
import qualified Data.ByteString.Lazy as LBS
import qualified Lucid
import qualified Okapi

instance Okapi.Writeable (Lucid.Html ()) where
  toLBS :: Lucid.Html () -> LBS.ByteString
  toLBS = Lucid.renderBS

emptyHtml :: Lucid.Html ()
emptyHtml = ""

writeLucid :: (Okapi.ServerM m, Lucid.ToHtml a) => a -> m ()
writeLucid htmlable = do
  Okapi.setHeader ("Content-Type", "text/html;charset=utf-8")
  Okapi.write $ toHtmlPure htmlable
  where
    toHtmlPure :: Lucid.ToHtml a => a -> Lucid.Html ()
    toHtmlPure = Lucid.toHtml
