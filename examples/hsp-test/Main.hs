{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Main where

import qualified Okapi
import Okapi
import Okapi.HSP (hsp)
import qualified Data.Text as Text
import qualified Control.Monad.Combinators as Combinators
import Control.Monad
import Control.Monad.Combinators
import Lucid
import Data.Text.Encoding as Text
import Data.ByteString.Lazy as LBS

toLBS :: Show a => a -> LBS.ByteString
toLBS = LBS.fromStrict . Text.encodeUtf8 . Text.pack . show

main :: IO ()
main = Okapi.run id [hsp|my_hsp_files|]
