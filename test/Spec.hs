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
import Okapi.Parser
import Okapi.Patterns
import Okapi.Types
import System.Environment (getArgs)
import Test.DocTest (mainFromCabal)
import Web.HttpApiData

runDoctests :: IO ()
runDoctests = mainFromCabal "okapi" =<< getArgs

type Okapi = OkapiT IO

{-
someRoute =
  [route|
  Okapi.Patterns.GET
  HEAD
  /movies
  /{Int|isModern}
  ?director{Text}
  ?actors{Text->childActors->bornInIndiana|notEmpty}
  ?female{Text}
  |]

someRouteHandler :: (Int, Text, Text, Text) -> Okapi Okapi.Response
someRouteHandler (_, _, _, _) = respond _200

someRoute2 =
  [route|
  Okapi.Patterns.GET
  HEAD
  /movies
  /{Int|isModern}
  ?director{Text}
  ?actors{Text->childActors->bornInIndiana|notEmpty}
  ?female{Text}
  >>= someRoute2Handler
  |]

someRoute2Handler :: (Int, Text, Text, Text) -> Okapi Okapi.Response
someRoute2Handler (_, _, _, _) = respond _200

someRoute3 =
  [route|
  Okapi.Patterns.GET
  /todos
  /{Int}
  >>= verifyTodo
  |]

verifyTodo :: Int -> Okapi Okapi.Response
verifyTodo id_ = if id_ > 0 then pure _200 else Okapi.throw _204

someRoute3TestSession :: Session ()
someRoute3TestSession = do
  testRequest (TestRequest methodGet [] "/todos/20" "")
    >>= assertStatus 200

  testRequest (TestRequest methodGet [] "/todos" "")
    >>= assertStatus 404

  testRequest (TestRequest methodGet [] "/todos/-1" "")
    >>= assertStatus 204

testSomeRoute3 :: IO ()
testSomeRoute3 = Okapi.runSession someRoute3TestSession id (parser someRoute3)

testSomeRoute :: IO ()
testSomeRoute = do
  let urlFunc = url someRoute
  print (urlFunc (5, "John", "World", "true"))
  let urlFunc2 = url someRoute2
  print (urlFunc (5, "John", "World", "true") == urlFunc2 (5, "John", "World", "true"))
-}
testServer :: Okapi Okapi.Response
testServer = do
  let parser1 = do
        get
        pathSeg "todos"
        respond _200

      parser2 = do
        get
        path ["todos", "completed"]
        respond _200

      parser3 = do
        get
        pathSeg "todos"
        status <- queryParam "status"
        _200
          & plaintext status
          & respond

      parser4 = do
        get
        pathSeg "a"
        respond _200

      parser5 = do
        get
        pathSeg "todos"
        queryFlag "progress"
        respond _200

      parser6 = do
        get
        pathSeg ""
        respond _200

  choice
    [ parser3,
      parser2,
      parser1,
      parser4,
      parser5,
      parser6
    ]

testServerQuasi :: Okapi Okapi.Response
testServerQuasi =
  choice
    [ parser3,
      parser2,
      parser1,
      parser4
    ]
  where
    parser1 = parser [route|Okapi.Patterns.GET /todos|] >> respond (_200 & plaintext "parser1")
    parser2 = parser [route|Okapi.Patterns.GET /todos /completed|] >> respond (_200 & plaintext "parser2")
    parser3 = parser [route|Okapi.Patterns.GET /todos ?status{Text}|] >>= (\status -> _200 & plaintext status & respond)
    parser4 = parser [route|Okapi.Patterns.GET /a|] >> respond (_200 & plaintext "parser4")

testSession :: Session ()
testSession = do
  testRequest (Request GET ["todos"] [] "" [])
    >>= assertStatus 200

  testRequest (Request POST ["todos"] [] "" [])
    >>= assertStatus 404

  testRequest (Request GET ["todos", "completed"] [] "" [])
    >>= assertStatus 200

  res3 <- testRequest $ Request GET ["todos"] [("status", QueryParam "done")] "" []
  assertStatus 200 res3
  assertBody "done" res3

pattern BlogRoute :: Okapi.Request
pattern BlogRoute <-
  Request Okapi.Patterns.GET ["blog"] _ _ _
  where
    BlogRoute = Request Okapi.Patterns.GET ["blog"] mempty mempty mempty

pattern BlogIDRoute :: Int -> Okapi.Request
pattern BlogIDRoute blogID <-
  Request Okapi.Patterns.GET ["blog", parseUrlPiece -> Right blogID] _ _ _
  where
    BlogIDRoute blogID = Request Okapi.Patterns.GET ["blog", toUrlPiece blogID] mempty mempty mempty

pattern BlogIDSectionRoute :: Int -> Text -> Okapi.Request
pattern BlogIDSectionRoute blogID sectionName <-
  Request Okapi.Patterns.GET ["blog", PathParam blogID, sectionName] _ _ _
  where
    -- TODO: NO NEED TO BE EXPLICIT HERE??
    BlogIDSectionRoute blogID sectionName = Request Okapi.Patterns.GET ["blog", PathParam blogID, sectionName] mempty mempty mempty

pattern BlogQueryParamsRoute :: (FromHttpApiData a1, FromHttpApiData a2, ToHttpApiData a1, ToHttpApiData a2) => a1 -> a2 -> Okapi.Query
pattern BlogQueryParamsRoute author category <-
  (viewQuery "author" -> (HasQueryParam author, viewQuery "category" -> (HasQueryParam category, _)))
  where
    BlogQueryParamsRoute author category = [("author", QueryParam $ toQueryParam author), ("category", QueryParam $ toQueryParam category)]

pattern BlogQueryRoute :: Text -> Text -> Okapi.Request
pattern BlogQueryRoute author category <-
  Request Okapi.Patterns.GET ["blog"] (BlogQueryParamsRoute author category) _ _
  where
    BlogQueryRoute author category = Request Okapi.Patterns.GET ["blog"] (BlogQueryParamsRoute author category) mempty mempty

-- | Test example patterns in Okapi.Request module
--
-- >>> parser = testMatcher
--
-- >>> result1 <- testParserIO parser (TestRequest "Okapi.Patterns.GET" [] "/blog" "")
-- >>> assertResponse is200 result1
-- True
-- >>> result2 <- testParserIO parser (TestRequest "Okapi.Patterns.GET" [] "/blog/2" "")
-- >>> assertResponse is200 result2
-- True
-- >>> result3 <- testParserIO parser (TestRequest "Okapi.Patterns.GET" [] "/blog/7/intro" "")
-- >>> assertResponse is200 result3
-- True
--
-- >>> result4 <- testParserIO parser (TestRequest "Okapi.Patterns.GET" [] "/blog?author=Diamond&category=pets" "")
-- >>> assertResponse is200 result4
-- True
--
-- >>> result5 <- testParserIO parser (TestRequest "Okapi.Patterns.GET" [] "/blog?author=Diamond" "")
-- >>> assertResponse is200 result5
-- False
testMatcher :: MonadOkapi m => m Okapi.Response
testMatcher = match $ \case
  BlogRoute -> respond _200
  BlogIDRoute blogID -> respond _200
  BlogIDSectionRoute blogID sectionName -> respond _200
  BlogQueryRoute author category -> respond _200
  _ -> Okapi.skip

testPattern :: (Okapi.Request -> Bool) -> Okapi.Request -> Bool
testPattern f = f

-- >>> testBlogPattern
-- True
testBlogPattern :: Bool
testBlogPattern = testPattern (\case BlogRoute -> True; _ -> False) BlogRoute

-- >>> testBlogIdPattern
-- True
testBlogIdPattern :: Bool
testBlogIdPattern = testPattern (\case BlogIDRoute 7 -> True; _ -> False) (BlogIDRoute 7)

main :: IO ()
main = do
  runDoctests
  Okapi.runSession testSession liftIO testServer
  Okapi.runSession testSession liftIO testServerQuasi
