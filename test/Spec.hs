{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

import Control.Monad.Combinators
import Control.Monad.IO.Class
import Control.Monad.Identity
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Function
import Data.Text
import Data.Text.Encoding
import Network.Wai
import Network.Wai.EventSource (ServerEvent (RetryEvent))
import Network.Wai.Test
import Okapi
import System.Environment (getArgs)
import Test.DocTest (mainFromCabal)
import Web.HttpApiData

runDoctests :: IO ()
runDoctests = mainFromCabal "okapi" =<< getArgs

main :: IO ()
main = runDoctests
