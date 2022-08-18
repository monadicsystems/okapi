{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

module Okapi.Pattern2 where

import Control.Monad
import Control.Monad.Combinators
import Control.Monad.IO.Class
import qualified Data.Attoparsec.Text as Atto
import Data.Either.Extra
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

data QueryValue = QueryParam Text | QueryFlag deriving (Eq, Show)

data Pattern = Pattern HTTP.Method [Text] (Map Text QueryValue) deriving (Eq, Show)

pattern GET :: HTTP.Method
pattern GET = "GET"

pattern POST :: HTTP.Method
pattern POST = "POST"

pattern DELETE :: HTTP.Method
pattern DELETE = "DELETE"

pattern OTHER :: HTTP.Method -> HTTP.Method
pattern OTHER method <-
  method
  where
    OTHER method = method

pattern PathParam :: (ToHttpApiData a, FromHttpApiData a) => a -> Text
pattern PathParam param <-
  (parseUrlPiece -> Right param)
  where
    PathParam param = toUrlPiece param

pattern NoQueryParams :: Map Text QueryValue
pattern NoQueryParams <-
  (Map.null -> True)
  where
    NoQueryParams = mempty

viewQuery :: Text -> Map Text QueryValue -> (Maybe QueryValue, Map Text QueryValue)
viewQuery queryParamName queryMap = case Map.lookup queryParamName queryMap of
  Nothing -> (Nothing, queryMap)
  just -> (just, Map.delete queryParamName queryMap)

pattern FoundQueryParam :: Text -> Maybe QueryValue
pattern FoundQueryParam queryParamValue <- Just (QueryParam queryParamValue)

pattern FoundQueryFlag :: Maybe QueryValue
pattern FoundQueryFlag <- Just QueryFlag

parsePattern :: MonadOkapi m => m Pattern
parsePattern = do
  method <- parseMethod
  path <- some parsePathSeg
  queryMap <- parseAllQueryItems
  pure $ Pattern method path (fmap (\case Just txt -> QueryParam txt; Nothing -> QueryFlag) queryMap) -- Should be a parser that parses everything

matchWith :: MonadOkapi m => (Pattern -> m Response) -> m Response
matchWith matcher = parsePattern >>= matcher

encodePattern :: Pattern -> URL
encodePattern p = undefined

decodePattern :: URL -> Maybe Pattern
decodePattern (URL "") = Nothing
decodePattern (URL url) = eitherToMaybe $
  flip Atto.parseOnly url $ do
    path <- many pathSeg
    maybeQueryStart <- optional $ Atto.char '?'
    case maybeQueryStart of
      Nothing -> pure $ Pattern GET path mempty
      Just _ -> do
        query <- Map.fromList <$> many queryParam
        pure $ Pattern GET path query
  where
    pathSeg :: Atto.Parser Text
    pathSeg = do
      Atto.char '/'
      Atto.takeWhile (\c -> c /= '/' && c /= '?')

    queryParam :: Atto.Parser (Text, QueryValue)
    queryParam = do
      queryParamName <- Atto.takeWhile (\c -> c /= '=' && c /= '&')
      mbEquals <- optional $ Atto.char '='
      case mbEquals of
        Nothing -> pure (queryParamName, QueryFlag)
        Just _ -> do
          queryParamValue <- Atto.takeWhile (/= '&')
          pure (queryParamName, QueryParam queryParamValue)

-- BELOW IS FOR TESTING

pattern BlogRoute :: Pattern
pattern BlogRoute <-
  Pattern GET ["blog"] (Map.null -> True)
  where
    BlogRoute = Pattern GET ["blog"] mempty

pattern BlogRouteId :: Int -> Pattern
pattern BlogRouteId blogID <-
  Pattern GET ["blog", parseUrlPiece -> Right blogID] (Map.null -> True)
  where
    BlogRouteId blogID = Pattern GET ["blog", toUrlPiece blogID] mempty

pattern BlogRouteIdSection :: Int -> Text -> Pattern
pattern BlogRouteIdSection blogID sectionName <-
  Pattern GET ["blog", PathParam blogID, sectionName] NoQueryParams
  where
    -- TODO: NO NEED TO BE EXPLICIT HERE??
    BlogRouteIdSection blogID sectionName = Pattern GET ["blog", PathParam blogID, sectionName] NoQueryParams

pattern BlogQueryRoute :: Text -> Text -> Pattern
pattern BlogQueryRoute author category <-
  Pattern
    GET
    ["blog"]
    (viewQuery "author" -> (FoundQueryParam author, viewQuery "category" -> (FoundQueryParam category, _)))
  where
    BlogQueryRoute author category = Pattern GET ["blog"] (Map.fromList [("author", QueryParam author), ("category", QueryParam category)])

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
testMatcher = matchWith $ \case
  BlogRoute -> respond _200
  BlogRouteId blogID -> respond _200
  BlogRouteIdSection blogID sectionName -> respond _200
  BlogQueryRoute author category -> respond _200
  _ -> Okapi.skip

testPattern :: (Pattern -> Bool) -> Pattern -> Bool
testPattern f = f

-- >>> testBlogPattern
-- True
testBlogPattern :: Bool
testBlogPattern = testPattern (\case BlogRoute -> True; _ -> False) BlogRoute

-- >>> testBlogIdPattern
-- True
testBlogIdPattern :: Bool
testBlogIdPattern = testPattern (\case BlogRouteId 7 -> True; _ -> False) (BlogRouteId 7)
