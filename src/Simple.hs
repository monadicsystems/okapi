{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module Simple where

import Data.Text
import Network.HTTP.Types (Method)
import Network.Wai
import Network.Wai.Internal
import qualified Result
import qualified Simple.Query as Query
import Web.HttpApiData


data PersonInfo = PersonInfo
    { familyName :: Text
    , salary :: Float
    }

personInfoP :: Query.Parser PersonInfo
personInfoP = do
    familyName <- Query.param @Text "family_name"
    salary <- Query.param @Float "salary"
    return PersonInfo{..}

app :: Application
app req res = case req of
    Request{requestMethod = GET, pathInfo = ["hello"]} -> do
        undefined
    Request
        { requestMethod = POST
        , pathInfo = ["tell", "me", Param @Text name]
        , queryString = Query.match personInfoP -> Result.Ok personInfo
        , requestHeaders = Headers.match personHeaders -> Result.Ok personCreds
        } -> do
            undefined
    Request{..} -> undefined
    _ -> undefined
