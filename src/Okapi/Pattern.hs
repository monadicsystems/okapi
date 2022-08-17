{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

module Okapi.Pattern where

import Control.Monad
import Control.Monad.Combinators
import Control.Monad.IO.Class
import Data.Map
import qualified Data.Map as Map
import Data.Text
import qualified Network.HTTP.Types as HTTP
import Okapi
import Okapi.Response
import Okapi.Types
import Web.HttpApiData

-- $setup
-- >>> :set -XFlexibleContexts
-- >>> :set -XOverloadedStrings
-- >>> import Okapi.Response
-- >>> import Okapi.Test

data Method
  = GET
  | POST
  | PUT
  | DELETE
  | OTHER
  deriving (Eq, Show)

data URLData = URLData Method [Text] (Map Text (Maybe Text)) deriving (Eq, Show)

pattern QueryParam :: a -> Maybe (Maybe a)
pattern QueryParam txt <- Just (Just txt)

pattern QueryFlag :: Maybe (Maybe a)
pattern QueryFlag = Just Nothing

httpMethodToMethod :: HTTP.Method -> Method
httpMethodToMethod = \case
  "GET" -> GET
  "POST" -> POST
  "PUT" -> PUT
  "DELETE" -> DELETE
  _ -> OTHER

getURLPattern :: MonadOkapi m => m URLData
getURLPattern = do
  method <- parseMethod
  path <- some parsePathSeg
  queryMap <- parseAllQueryItems
  pure $ URLData (httpMethodToMethod method) path queryMap -- Should be a parser that parses everything

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
pattern BlogRoute <-
  URLData GET ["blog"] (Map.null -> True)
  where
    BlogRoute = URLData GET ["blog"] mempty

pattern BlogRouteId :: Int -> URLData
pattern BlogRouteId blogID <-
  URLData GET ["blog", parseUrlPiece -> Right blogID] (Map.null -> True)
  where
    BlogRouteId blogID = URLData GET ["blog", toUrlPiece blogID] mempty

pattern BlogRouteIdSection :: Int -> Text -> URLData
pattern BlogRouteIdSection blogID sectionName <-
  URLData GET ["blog", parseUrlPiece -> Right blogID, sectionName] (Map.null -> True)
  where
    BlogRouteIdSection blogID sectionName = URLData GET ["blog", toUrlPiece blogID, sectionName] mempty

viewQuery :: Text -> Map Text (Maybe Text) -> (Maybe (Maybe Text), Map Text (Maybe Text))
viewQuery queryParamName queryMap = case Map.lookup queryParamName queryMap of
  Nothing -> (Nothing, queryMap)
  just -> (just, Map.delete queryParamName queryMap)

pattern BlogQueryRoute :: Text -> Text -> URLData
pattern BlogQueryRoute author category <-
  URLData GET ["blog"] (viewQuery "author" -> (QueryParam author, viewQuery "category" -> (QueryParam category, _)))
  where
    BlogQueryRoute author category = URLData GET ["blog"] (Map.fromList [("author", Just author), ("category", Just category)])

-- | Test example patterns in Okapi.Pattern module
--
-- >>> parser = testMatcher
--
-- >>> result1 <- testParserIO parser (TestRequest "GET" [] "/blog" "")
-- >>> assertResponse is200 result1
-- True
-- >>> result2 <- testParserIO parser (TestRequest "GET" [] "/blog/2" "")
-- >>> assertResponse is200 result2
-- True
-- >>> result3 <- testParserIO parser (TestRequest "GET" [] "/blog/7/intro" "")
-- >>> assertResponse is200 result3
-- True
--
-- >>> result4 <- testParserIO parser (TestRequest "GET" [] "/blog?author=Diamond&category=pets" "")
-- >>> assertResponse is200 result4
-- True
--
-- >>> result5 <- testParserIO parser (TestRequest "GET" [] "/blog?author=Diamond" "")
-- >>> assertResponse is200 result5
-- False
testMatcher :: MonadOkapi m => m Response
testMatcher = matchURLWith $ \case
  BlogRoute -> respond _200
  BlogRouteId blogID -> respond _200
  BlogRouteIdSection blogID sectionName -> respond _200
  BlogQueryRoute author category -> respond _200
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
