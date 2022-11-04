type Okapi = ServerT IO

testServer :: Okapi Okapi.Effect.Response
testServer = do
  let parser1 = do
        get
        pathSeg "todos"
        respond ok

      parser2 = do
        get
        path ["todos", "completed"]
        respond ok

      parser3 = do
        get
        pathSeg "todos"
        status <- queryParam "status"
        ok
          & plaintext status
          & respond

      parser4 = do
        get
        pathSeg "a"
        respond ok

      parser5 = do
        get
        pathSeg "todos"
        queryFlag "progress"
        respond ok

      parser6 = do
        get
        pathSeg ""
        respond ok

  choice
    [ parser3,
      parser2,
      parser1,
      parser4,
      parser5,
      parser6
    ]

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

pattern BlogRoute :: Okapi.Effect.Request
pattern BlogRoute <-
  Request Okapi.Patterns.GET ["blog"] _ _ _
  where
    BlogRoute = Request Okapi.Patterns.GET ["blog"] mempty mempty mempty

pattern BlogIDRoute :: Int -> Okapi.Effect.Request
pattern BlogIDRoute blogID <-
  Request Okapi.Patterns.GET ["blog", parseUrlPiece -> Right blogID] _ _ _
  where
    BlogIDRoute blogID = Request Okapi.Patterns.GET ["blog", toUrlPiece blogID] mempty mempty mempty

pattern BlogIDSectionRoute :: Int -> Text -> Okapi.Effect.Request
pattern BlogIDSectionRoute blogID sectionName <-
  Request Okapi.Patterns.GET ["blog", PathParam blogID, sectionName] _ _ _
  where
    -- TODO: NO NEED TO BE EXPLICIT HERE??
    BlogIDSectionRoute blogID sectionName = Request Okapi.Patterns.GET ["blog", PathParam blogID, sectionName] mempty mempty mempty

pattern BlogQueryParamsRoute :: (FromHttpApiData a1, FromHttpApiData a2, ToHttpApiData a1, ToHttpApiData a2) => a1 -> a2 -> Okapi.Query
pattern BlogQueryParamsRoute author category <-
  (viewQuery "author" -> (IsQueryParam author, viewQuery "category" -> (IsQueryParam category, _)))
  where
    BlogQueryParamsRoute author category = [("author", QueryParam $ toQueryParam author), ("category", QueryParam $ toQueryParam category)]

pattern BlogQueryRoute :: Text -> Text -> Okapi.Effect.Request
pattern BlogQueryRoute author category <-
  Request Okapi.Patterns.GET ["blog"] (BlogQueryParamsRoute author category) _ _
  where
    BlogQueryRoute author category = Request Okapi.Patterns.GET ["blog"] (BlogQueryParamsRoute author category) mempty mempty

-- | Test example patterns in Okapi.Effect.Request module
--
-- >>> parser = testMatcher
--
-- >>> result1 <- testIO parser (TestRequest "Okapi.Patterns.GET" [] "/blog" "")
-- >>> assertResponse is200 result1
-- True
-- >>> result2 <- testIO parser (TestRequest "Okapi.Patterns.GET" [] "/blog/2" "")
-- >>> assertResponse is200 result2
-- True
-- >>> result3 <- testIO parser (TestRequest "Okapi.Patterns.GET" [] "/blog/7/intro" "")
-- >>> assertResponse is200 result3
-- True
--
-- >>> result4 <- testIO parser (TestRequest "Okapi.Patterns.GET" [] "/blog?author=Diamond&category=pets" "")
-- >>> assertResponse is200 result4
-- True
--
-- >>> result5 <- testIO parser (TestRequest "Okapi.Patterns.GET" [] "/blog?author=Diamond" "")
-- >>> assertResponse is200 result5
-- False
testMatcher :: MonadHTTP m => m Okapi.Effect.Response
testMatcher = match $ \case
  BlogRoute -> respond ok
  BlogIDRoute blogID -> respond ok
  BlogIDSectionRoute blogID sectionName -> respond ok
  BlogQueryRoute author category -> respond ok
  _ -> Okapi.next

testPattern :: (Okapi.Effect.Request -> Bool) -> Okapi.Effect.Request -> Bool
testPattern f = f

-- >>> testBlogPattern
-- True
testBlogPattern :: Bool
testBlogPattern = testPattern (\case BlogRoute -> True; _ -> False) BlogRoute

-- >>> testBlogIdPattern
-- True
testBlogIdPattern :: Bool
testBlogIdPattern = testPattern (\case BlogIDRoute 7 -> True; _ -> False) (BlogIDRoute 7)
