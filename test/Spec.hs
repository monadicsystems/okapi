{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

import Control.Monad.Combinators
import Control.Monad.IO.Class
import Control.Monad.Identity
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Function
import Data.Text
import Data.Text.Encoding
import Network.HTTP.Types
import Network.Wai
import Network.Wai.EventSource (ServerEvent (RetryEvent))
import Network.Wai.Test
import Okapi
import qualified Okapi
import Web.HttpApiData

import Test.DocTest (mainFromCabal)
import System.Environment (getArgs)

runDoctests :: IO ()
runDoctests = mainFromCabal "okapi" =<< getArgs

type Okapi = OkapiT IO

someRoute = [route|
  GET
  HEAD
  /movies
  /{Int|isModern}
  ?director{Text}
  ?actors{Text->childActors->bornInIndiana|notEmpty}
  ?female{Text}
  >>= someRouteHandler
  |]

someRouteHandler :: (Int, Text, Text, Text) -> Okapi Okapi.Response
someRouteHandler (_, _, _, _) = respond ok

someRoute2 =
  [route|
  GET
  HEAD
  /movies
  /{Int|isModern}
  ?director{Text}
  ?actors{Text->childActors->bornInIndiana|notEmpty}
  ?female{Text}
  >>= someRoute2Handler
  |]

someRoute2Handler :: (Int, Text, Text, Text) -> Okapi Okapi.Response
someRoute2Handler (_, _, _, _) = respond ok

someRoute3 =
  [route|
  GET
  /todos
  /{Int}
  >>= verifyTodo
  |]

verifyTodo :: Int -> Okapi Okapi.Response
verifyTodo id_ = if id_ > 0 then pure ok else Okapi.throw noContent

someRoute3TestSession :: Session ()
someRoute3TestSession = do
  send (TestRequest methodGet [] "/todos/20" "")
    >>= assertStatus 200

  send (TestRequest methodGet [] "/todos" "")
    >>= assertStatus 404

  send (TestRequest methodGet [] "/todos/-1" "")
    >>= assertStatus 204

testSomeRoute3 :: IO ()
testSomeRoute3 = Okapi.runSession someRoute3TestSession id (parser someRoute3)

testSomeRoute :: IO ()
testSomeRoute = do
  let urlFunc = url someRoute
  print (urlFunc (5, "John", "World", "true"))
  let urlFunc2 = url someRoute2
  print (urlFunc (5, "John", "World", "true") == urlFunc2 (5, "John", "World", "true"))

testServer :: Okapi Okapi.Response
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
    [ parser1,
      parser2,
      parser3,
      parser4,
      parser5,
      parser6
    ]

testServerQuasi :: Okapi Okapi.Response
testServerQuasi = choice
 [ parser1
 , parser2
 , parser3
 , parser4
 ]
 where
  parser1 = parser [route|GET /todos|] >> respond ok
  parser2 = parser [route|GET /todos /completed|] >> respond ok
  parser3 = parser [route|GET /todos ?status{Text}|] >>= (\status -> ok & plaintext status & respond)
  parser4 = parser [route|GET /a|] >> respond ok

testSession :: Session ()
testSession = do
  send (TestRequest methodGet [] "/todos" "")
    >>= assertStatus 200

  send (TestRequest methodPost [] "/todos" "")
    >>= assertStatus 404

  send (TestRequest methodGet [] "/todos/completed" "")
    >>= assertStatus 200

  res3 <- send $ TestRequest methodGet [] "/todos?status=done" ""
  assertStatus 200 res3
  assertBody "done" res3

  -- send (TestRequest methodGet [] "/todos?progress" "")
  --   >>= assertStatus 200

  send (TestRequest methodGet [] "/todos?what" "")
    >>= assertStatus 404

  send (TestRequest methodGet [] "/what" "")
    >>= assertStatus 404

  send (TestRequest methodGet [] "/a" "")
    >>= assertStatus 200

-- testSession2 = do
--   send (TestRequest methodGet [] "/")

-- send (TestRequest methodGet [] "/" "") ?? Maybe because of how path is stored in srequest
--   >>= assertStatus 200
{-
test1 :: IO ()
test1 = do
  let result = parseOnly routeParser "GET HEAD /movies /{Date|isModern} ?director{Director} ?actors{[Actor]->childActors->bornInIndiana|notEmpty} ?female{Gender}"
      goal =
        Right
          [ Method "GET",
            Method "HEAD",
            PathSegMatch "movies",
            AnonPathSeg (CurlyExpr "Date" [] (Just "isModern")),
            AnonQueryParam "director" (CurlyExpr "Director" [] Nothing),
            AnonQueryParam "actors" (CurlyExpr "[Actor]" ["childActors", "bornInIndiana"] (Just "notEmpty")),
            AnonQueryParam "female" (CurlyExpr "Gender" [] Nothing)
          ]
  if result == goal
    then print "PASSED!"
    else print "FAILED!"
-}

main :: IO ()
main = do
  runDoctests
  testSomeRoute
  testSomeRoute3
  Okapi.runSession testSession liftIO testServer
  Okapi.runSession testSession liftIO testServerQuasi

-- Okapi.Test.runSession testSession2 liftIO (parser someRoute2)
