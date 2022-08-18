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
import System.Environment (getArgs)
import Test.DocTest (mainFromCabal)
import Web.HttpApiData

runDoctests :: IO ()
runDoctests = mainFromCabal "okapi" =<< getArgs

type Okapi = OkapiT IO

{-
someRoute =
  [route|
  GET
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
someRoute2Handler (_, _, _, _) = respond _200

someRoute3 =
  [route|
  GET
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
    parser1 = parser [route|GET /todos|] >> respond (_200 & plaintext "parser1")
    parser2 = parser [route|GET /todos /completed|] >> respond (_200 & plaintext "parser2")
    parser3 = parser [route|GET /todos ?status{Text}|] >>= (\status -> _200 & plaintext status & respond)
    parser4 = parser [route|GET /a|] >> respond (_200 & plaintext "parser4")

testSession :: Session ()
testSession = do
  testRequest (TestRequest methodGet [] "/todos" "")
    >>= assertStatus 200

  testRequest (TestRequest methodPost [] "/todos" "")
    >>= assertStatus 404

  testRequest (TestRequest methodGet [] "/todos/completed" "")
    >>= assertStatus 200

  res3 <- testRequest $ TestRequest methodGet [] "/todos?status=done" ""
  assertStatus 200 res3
  assertBody "done" res3

main :: IO ()
main = do
  runDoctests
  Okapi.runSession testSession liftIO testServer
  Okapi.runSession testSession liftIO testServerQuasi
