{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

import Control.Exception (evaluate)
import Data.Aeson qualified as Aeson
import Data.OpenApi qualified as OAPI
import Data.Text qualified as Text
import Network.HTTP.Types qualified as HTTP
import Okapi.Endpoint
import Okapi.Script
import Okapi.Script.Body qualified as Body
import Okapi.Script.Headers qualified as Headers
import Okapi.Script.Path qualified as Path
import Okapi.Script.Query qualified as Query
import Okapi.Script.Responder qualified as Responder
import Okapi.Script.ResponderHeaders (Response)
import Okapi.Script.ResponderHeaders qualified as ResponderHeaders
import Prelude.Linear qualified as L
import Test.Hspec
import Web.HttpApiData qualified as Web

main :: IO ()
main = hspec $ do
  describe "Path Operations" $ do
    it "returns the first element of a list" $ do
      Path.eval path1 ["index"] `shouldBe` (Ok (), [])

    it "returns the first element of a list" $ do
      Path.eval path1 ["index", "about"] `shouldBe` (Fail Path.NotEnoughOperations, ["index", "about"])

    it "returns the first element of a list" $ do
      Path.eval path2 ["item", "5"] `shouldBe` (Ok 5, [])

    it "returns the first element of a list" $ do
      Path.eval path2 ["item"] `shouldBe` (Fail Path.TooManyOperations, ["item"])

    it "returns the first element of a list" $ do
      Path.eval path3 ["product", "books", "56708"] `shouldBe` (Ok (Category "books", ProductID 56708), [])

    it "returns the first element of a list" $ do
      Path.eval path3 ["product", "books", "56708", "info"] `shouldBe` (Fail Path.NotEnoughOperations, ["product", "books", "56708", "info"])

    it "returns the first element of a list" $ do
      Path.eval path3 ["product", "books"] `shouldBe` (Fail Path.TooManyOperations, ["product", "books"])

  describe "Query Operations" $ do
    it "returns the first element of a list" $ do
      Query.eval query1 [("score", Just "5"), ("user", Just "Joe")] `shouldBe` (Ok $ Filter 5 $ Username "Joe", [])

  it "returns the first element of a list" $ do
    Query.eval query2 [("user", Just "Bob"), ("active", Nothing)] `shouldBe` (Ok $ Username "Bob", [])

  it "returns the first element of a list" $ do
    Query.eval query3 [("username", Just "Bob")] `shouldBe` (Ok (Username "Anon", Nothing), [("username", Just "Bob")])

data AddHeaders = AddHeaders
  { addCookie :: Username -> Response -> Response,
    addAnotherHeader :: Username -> Response -> Response,
    cacheHeader :: Int -> Response -> Response
  }

responder1 = do
  itsOk <- Responder.json
    @Aeson.Value
    HTTP.status200
    do
      addCookie <- ResponderHeaders.has @Username "Cookie"
      pure addCookie
  itsNotFound <- Responder.json
    @Aeson.Value
    HTTP.status404
    do
      addCookie <- ResponderHeaders.has @Username "Blob"
      addAnotherHeader <- ResponderHeaders.has @Username "X-Some-Header"
      cacheHeader <- ResponderHeaders.has @Int "X-Cache-Time"
      pure $ AddHeaders {..}
  pure (itsOk, itsNotFound)

responder2 = undefined

newtype Username = Username {unwrap :: Text.Text}
  deriving newtype (Eq, Show, Web.FromHttpApiData, Web.ToHttpApiData, OAPI.ToSchema, Aeson.ToJSON)

data Filter = Filter
  { score :: Int,
    byUser :: Username
  }
  deriving (Eq, Show)

query1 :: Query.Script Filter
query1 = do
  score <- Query.param @Int "score"
  byUser <- Query.param @Username "user"
  pure Filter {..}

query2 :: Query.Script Username
query2 = do
  username <- Query.param @Username "user"
  Query.flag "active"
  pure username

query3 :: Query.Script (Username, Maybe ())
query3 = do
  user <- Query.option (Username "Anon") $ Query.param "user"
  active <- Query.optional $ Query.flag "active"
  pure (user, active)

path1 :: Path.Script ()
path1 = Path.static "index"

path2 :: Path.Script Int
path2 = do
  Path.static "item"
  uuid <- Path.param @Int "uuid"
  pure uuid

newtype Category = Category {unwrap :: Text.Text}
  deriving newtype (Eq, Show, Web.FromHttpApiData, OAPI.ToSchema, Aeson.ToJSON)

newtype ProductID = ProductID {unwrap :: Int}
  deriving newtype (Eq, Show, Web.FromHttpApiData, OAPI.ToSchema, Aeson.ToJSON)

path3 :: Path.Script (Category, ProductID)
path3 = do
  Path.static "product"
  category <- Path.param @Category "category"
  productID <- Path.param @ProductID "productID"
  pure (category, productID)

testPlan =
  Plan
    id
    ( Endpoint
        HTTP.GET
        do
          Path.static "index"
          magicNumber <- Path.param @Int "magicNumber"
          pure magicNumber
        do
          x <- Query.param @Int "x"
          y <- Query.option 10 $ Query.param @Int "y"
          pure (x, y)
        do pure ()
        do pure ()
        do
          itsOk <- Responder.json @Int HTTP.status200 do
            addSecretNumber <- ResponderHeaders.has @Int "X-SECRET"
            pure addSecretNumber
          pure itsOk
    )
    \magicNumber (x, y) () () responder ->
      do
        let newNumber = magicNumber + x * y
        print newNumber
        return $ responder (\addHeader response -> addHeader (newNumber * 100) response) newNumber
