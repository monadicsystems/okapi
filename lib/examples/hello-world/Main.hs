{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import qualified Data.Text as Text
import qualified Network.HTTP.Types as HTTP
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import Okapi.App
import Okapi.Response
import qualified Web.HttpApiData as Web

helloWorld =
    responder @200 @'[] @Text.Text @Text.Text
        . method HTTP.GET id
        $ \greet _req -> return $ greet noHeaders "Hello World!"

main =
    Warp.run 8000
        . withDefault helloWorld
        $ \_ resp -> resp $ Wai.responseLBS HTTP.status404 [] "Not Found..."