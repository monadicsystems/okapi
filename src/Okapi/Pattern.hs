{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

module Okapi.Pattern where

import Control.Monad.IO.Class
import Data.Map
import Data.Text
import qualified Network.HTTP.Types as HTTP
import Okapi
import Okapi.Response
import Okapi.Types
import Web.HttpApiData

data Method
  = GET
  | POST
  | PUT
  | DELETE
  deriving (Eq, Show)

data URLData = URLData Method [Text] deriving (Eq, Show)

getURLPattern :: MonadOkapi m => m URLData
getURLPattern = pure $ URLData GET ["home"] -- Should be a parser that parses everything

matchURLWith :: MonadOkapi m => (URLData -> m Response) -> m Response
matchURLWith matcher = do
  urlPattern <- getURLPattern
  matcher urlPattern

renderURLData :: URLData -> URL
renderURLData urlData = undefined

parseURLToURLData :: MonadOkapi m => URL -> m URLData
parseURLToURLData url = undefined

testURLPatternRoundtrip :: MonadOkapi m => URLData -> m Bool
testURLPatternRoundtrip urlData = do
  let url = renderURLData urlData
  parsedUrlData <- parseURLToURLData url
  pure $ urlData == parsedUrlData

-- BELOW IS FOR TESTING

pattern BlogRoute :: URLData
pattern BlogRoute = URLData GET ["blog"]

pattern BlogRouteId :: Int -> URLData
pattern BlogRouteId blogID <-
  URLData GET ["blog", parseUrlPiece -> Right blogID]
  where
    BlogRouteId blogID = URLData GET ["blog", toUrlPiece blogID]

pattern BlogRouteIdSection :: Int -> Text -> URLData
pattern BlogRouteIdSection blogID sectionName <-
  URLData GET ["blog", parseUrlPiece -> Right blogID, sectionName]
  where
    BlogRouteIdSection blogID sectionName = URLData GET ["blog", toUrlPiece blogID, sectionName]

-- | Test example patterns in Okapi.Pattern module TODO:::NOW !!!!!!!
--
-- >>> :{
-- parser = do
--   get
--   pathSeg "users"
--   isAdmin <- queryFlag "admin"
--   respond $
--     if isAdmin
--       then json ["Derek", "Alice"] $ _200
--       else json ["Derek", "Alice", "Bob", "Casey", "Alex", "Larry"] $ _200
-- :}
--
-- >>> result1 <- testParserIO parser (TestRequest "GET" [] "/users?admin" "")
-- >>> assertResponse (hasBodyRaw "[\"Derek\",\"Alice\"]") result1
-- True
-- >>> result2 <- testParserIO parser (TestRequest "GET" [] "/users?admin=foobarbaz" "")
-- >>> assertResponse (hasBodyRaw "[\"Derek\",\"Alice\"]") result2
-- True
-- >>> result3 <- testParserIO parser (TestRequest "GET" [] "/users" "")
-- >>> assertResponse (hasBodyRaw "[\"Derek\",\"Alice\",\"Bob\",\"Casey\",\"Alex\",\"Larry\"]") result3
-- True
testMatcher :: MonadOkapi m => m Response
testMatcher = matchURLWith $ \case
  BlogRoute -> respond _200
  BlogRouteId blogID -> do
    liftIO $ print blogID
    respond _200
  BlogRouteIdSection blogID sectionName -> do
    liftIO $ print blogID
    liftIO $ print sectionName
    respond _200
  _ -> skip

testPattern :: (URLData -> Bool) -> URLData -> Bool
testPattern f = f

-- >>> testBlogPattern
-- True
testBlogPattern :: Bool
testBlogPattern = testPattern (\case BlogRoute -> True; _ -> False) BlogRoute

-- >>> testBlogIdPattern
-- True
testBlogIdPattern :: Bool
testBlogIdPattern = testPattern (\case BlogRouteId 7 -> True; _ -> False) (BlogRouteId 7)
