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

someRouteHandler :: (Int, Text, Text, Text) -> Okapi Okapi.Effect.Response
someRouteHandler (_, _, _, _) = respond ok

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

someRoute2Handler :: (Int, Text, Text, Text) -> Okapi Okapi.Effect.Response
someRoute2Handler (_, _, _, _) = respond ok

someRoute3 =
  [route|
  Okapi.Patterns.GET
  /todos
  /{Int}
  >>= verifyTodo
  |]

verifyTodo :: Int -> Okapi Okapi.Effect.Response
verifyTodo id_ = if id_ > 0 then pure ok else Okapi.throw _204

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

testServerQuasi :: Okapi Okapi.Effect.Response
testServerQuasi =
  choice
    [ parser3,
      parser2,
      parser1,
      parser4
    ]
  where
    parser1 = parser [route|Okapi.Patterns.GET /todos|] >> respond (ok & plaintext "parser1")
    parser2 = parser [route|Okapi.Patterns.GET /todos /completed|] >> respond (ok & plaintext "parser2")
    parser3 = parser [route|Okapi.Patterns.GET /todos ?status{Text}|] >>= (\status -> ok & plaintext status & respond)
    parser4 = parser [route|Okapi.Patterns.GET /a|] >> respond (ok & plaintext "parser4")
