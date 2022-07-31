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
import Okapi.QuasiQuotes
import qualified Okapi.QuasiQuotes as Okapi
import Okapi.Test
import Web.HttpApiData

type Okapi = OkapiT IO

someRoute = [genRoute|GET HEAD /movies /{Int|isModern} ?director{Text} ?actors{Text->childActors->bornInIndiana|notEmpty} ?female{Text}|]

someRoute2 =
  [genRoute|
  GET
  HEAD
  /movies
  /{Int|isModern}
  ?director{Text}
  ?actors{Text->childActors->bornInIndiana|notEmpty}
  ?female{Text}
  >>= Okapi.respond
  |]

myResponse = undefined

testSomeRoute :: IO ()
testSomeRoute = do
  let urlFunc = url someRoute
  putStrLn $ show $ urlFunc (5, "John", "World", "true")
  let urlFunc2 = url someRoute2
  putStrLn $ show $ urlFunc (5, "John", "World", "true") == urlFunc2 (5, "John", "World", "true")

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

  send (TestRequest methodGet [] "/todos?progress" "")
    >>= assertStatus 200

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

main :: IO ()
main = do
  testSomeRoute
  Okapi.Test.runSession testSession liftIO testServer

-- Okapi.Test.runSession testSession2 liftIO (parser someRoute2)
