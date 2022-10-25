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
import Okapi.Effect.Response
import Okapi.Types
import Web.HttpApiData

-- $setup
-- >>> :set -XFlexibleContexts
-- >>> :set -XOverloadedStrings
-- >>> import Okapi.Effect.Response
-- >>> import Okapi.Test

type Query = Map Text QueryValue

data QueryValue = QueryParam Text | QueryFlag deriving (Eq, Show) -- QueryList [Text]

data Pattern = Pattern HTTP.Method [Text] Query deriving (Eq, Show)

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

pattern IsQueryParam :: FromHttpApiData a => a -> Maybe QueryParam
pattern IsQueryParam value <- Just (QueryParam (parseUrlPiece -> Just value))

pattern HasQueryFlag :: Maybe QueryParam
pattern HasQueryFlag <- Just QueryFlag

pattern NoQueryParams :: Query
pattern NoQueryParams <-
  (null -> True)
  where
    NoQueryParams = mempty

viewQuery :: Text -> Query -> (Maybe QueryValue, Query)
viewQuery name query = case Map.lookup name query of
  Nothing -> (Nothing, query)
  Just value -> (Just value, Map.delete name query)

parsePattern :: ServerM m => m Pattern
parsePattern = do
  method <- parseMethod
  path <- some parsePathSeg
  query <- fmap (\case Just txt -> QueryParam txt; Nothing -> QueryFlag) parseAllQueryItems
  pure $ Pattern method path query

matchWith :: ServerM m => (Pattern -> m Response) -> m Response
matchWith matcher = parsePattern >>= matcher

{-
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
-}

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
    (viewQuery "author" -> (IsQueryParam author, viewQuery "category" -> (IsQueryParam category, _)))
  where
    BlogQueryRoute author category = Pattern GET ["blog"] (Map.fromList [("author", QueryParam author), ("category", QueryParam category)])

-- | Test example patterns in Okapi.Pattern module
--
-- >>> parser = testMatcher
--
-- >>> result1 <- testIO parser (TestRequest "GET" [] "/blog" "")
-- >>> assertResponse is200 result1
-- True
-- >>> result2 <- testIO parser (TestRequest "GET" [] "/blog/2" "")
-- >>> assertResponse is200 result2
-- True
-- >>> result3 <- testIO parser (TestRequest "GET" [] "/blog/7/intro" "")
-- >>> assertResponse is200 result3
-- True
--
-- >>> result4 <- testIO parser (TestRequest "GET" [] "/blog?author=Diamond&category=pets" "")
-- >>> assertResponse is200 result4
-- True
--
-- >>> result5 <- testIO parser (TestRequest "GET" [] "/blog?author=Diamond" "")
-- >>> assertResponse is200 result5
-- False
testMatcher :: ServerM m => m Response
testMatcher = matchWith $ \case
  BlogRoute -> respond ok
  BlogRouteId blogID -> respond ok
  BlogRouteIdSection blogID sectionName -> respond ok
  BlogQueryRoute author category -> respond ok
  _ -> Okapi.next

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
