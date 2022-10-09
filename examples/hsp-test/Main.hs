{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Main where

import Okapi
import Okapi.HSP (hsp)
import qualified Data.Text as Text
import qualified Control.Monad.Combinators as Combinators
import Lucid

main :: IO ()
main = Okapi.run id [hsp|my_hsp_files|]
