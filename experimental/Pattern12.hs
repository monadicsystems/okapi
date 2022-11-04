{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

module Okapi.Pattern where

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

data Pattern = Pattern HTTP.Method [Text] deriving (Eq, Show)

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

parsePattern :: MonadHTTP m => m Pattern
parsePattern = do
  method <- parseMethod
  path <- many parsePathSeg
  pure $ Pattern method path

matchWith :: MonadHTTP m => (Pattern -> m Response) -> m Response
matchWith matcher = parsePattern >>= matcher

encodePattern :: Pattern -> URL
encodePattern p = undefined

decodePattern :: URL -> Maybe Pattern
decodePattern (URL "") = Nothing
decodePattern (URL url) =
  eitherToMaybe $
    flip Atto.parseOnly url $ Pattern GET <$> many pathSeg
  where
    pathSeg :: Atto.Parser Text
    pathSeg = do
      Atto.char '/'
      Atto.takeWhile (\c -> c /= '/' && c /= '?')

-- BELOW IS FOR TESTING

pattern BlogRoute :: Pattern
pattern BlogRoute = Pattern GET ["blog"]

pattern BlogRouteId :: Int -> Pattern
pattern BlogRouteId blogID = Pattern GET ["blog", PathParam blogID]

pattern BlogRouteIdSection :: Int -> Text -> Pattern
pattern BlogRouteIdSection blogID sectionName =
  Pattern GET ["blog", PathParam blogID, sectionName]

-- | Test example patterns in Okapi.Pattern module
--
-- >>> result1 <- testIO testMatcher (TestRequest "GET" [] "/blog" "")
-- >>> assertResponse is200 result1
-- True
-- >>> result2 <- testIO testMatcher (TestRequest "GET" [] "/blog/2" "")
-- >>> assertResponse is200 result2
-- True
-- >>> result3 <- testIO testMatcher (TestRequest "GET" [] "/blog/7/intro" "")
-- >>> assertResponse is200 result3
-- True
-- >>> result4 <- testIO testMatcher (TestRequest "GET" [] "/blog?author=Diamond&category=pets" "")
-- >>> assertResponse is200 result4
-- True
-- >>> result5 <- testIO testMatcher (TestRequest "GET" [] "/blog?author=Johnson" "")
-- >>> assertResponse is200 result5
-- True
testMatcher :: MonadHTTP m => m Response
testMatcher = matchWith $ \case
  BlogRoute -> respond ok
  BlogRouteId blogID -> respond ok
  BlogRouteIdSection blogID sectionName -> respond ok
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

-- $setup
-- >>> :set -XFlexibleContexts
-- >>> :set -XOverloadedStrings
-- >>> import Okapi.Effect.Response
-- >>> import Okapi.Test

{-
encodePattern :: Request -> URL
encodePattern p = undefined

decodePattern :: URL -> Maybe Request
decodePattern (URL "") = Nothing
decodePattern (URL url) = eitherToMaybe $
  flip Atto.parseOnly url $ do
    path <- many pathSeg
    maybeQueryStart <- optional $ Atto.char '?'
    case maybeQueryStart of
      Nothing -> pure $ Request GET path mempty
      Just _ -> do
        query <- Map.fromList <$> many queryParam
        pure $ Request GET path query
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
