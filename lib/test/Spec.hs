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
import Okapi.Operation
import Okapi.Spec
import Okapi.Spec.Request.Headers (Response)
import Okapi.Spec.Request.Headers qualified as AddHeaders
import Okapi.Spec.Request.Body qualified as Body
import Okapi.Spec.Request.Headers qualified as Headers
import Okapi.Spec.Request.Query qualified as Query
import Okapi.Spec.Response qualified as Response
import Okapi.Spec.Request.Route qualified as Route
import Prelude.Linear qualified as L
import Test.Hspec
import Web.HttpApiData qualified as Web

main :: IO ()
main = hspec $ do
  describe "Route Operations" $ do
    it "returns the first element of a list" $ do
      Route.eval path1 ["index"] `shouldBe` (Ok (), [])

    it "returns the first element of a list" $ do
      Route.eval path1 ["index", "about"] `shouldBe` (Fail Route.NotEnoughOperations, ["index", "about"])

    it "returns the first element of a list" $ do
      Route.eval path2 ["item", "5"] `shouldBe` (Ok 5, [])

    it "returns the first element of a list" $ do
      Route.eval path2 ["item"] `shouldBe` (Fail Route.TooManyOperations, ["item"])

    it "returns the first element of a list" $ do
      Route.eval path3 ["product", "books", "56708"] `shouldBe` (Ok (Category "books", ProductID 56708), [])

    it "returns the first element of a list" $ do
      Route.eval path3 ["product", "books", "56708", "info"] `shouldBe` (Fail Route.NotEnoughOperations, ["product", "books", "56708", "info"])

    it "returns the first element of a list" $ do
      Route.eval path3 ["product", "books"] `shouldBe` (Fail Route.TooManyOperations, ["product", "books"])

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
  itsOk <- Response.json
    @Aeson.Value
    HTTP.status200
    do
      addCookie <- AddHeaders.using @Username "Cookie"
      pure addCookie
  itsNotFound <- Response.json
    @Aeson.Value
    HTTP.status404
    do
      addCookie <- AddHeaders.using @Username "Blob"
      addAnotherHeader <- AddHeaders.using @Username "X-Some-Headers"
      cacheHeader <- AddHeaders.using @Int "X-Cache-Time"
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

query1 :: Query.Spec Filter
query1 = do
  score <- Query.param @Int "score"
  byUser <- Query.param @Username "user"
  pure Filter {..}

query2 :: Query.Spec Username
query2 = do
  username <- Query.param @Username "user"
  Query.flag "active"
  pure username

query3 :: Query.Spec (Username, Maybe ())
query3 = do
  user <- Query.option (Username "Anon") $ Query.param "user"
  active <- Query.optional $ Query.flag "active"
  pure (user, active)

path1 :: Route.Spec ()
path1 = Route.static "index"

path2 :: Route.Spec Int
path2 = do
  Route.static "item"
  uuid <- Route.param @Int "uuid"
  pure uuid

newtype Category = Category {unwrap :: Text.Text}
  deriving newtype (Eq, Show, Web.FromHttpApiData, OAPI.ToSchema, Aeson.ToJSON)

newtype ProductID = ProductID {unwrap :: Int}
  deriving newtype (Eq, Show, Web.FromHttpApiData, OAPI.ToSchema, Aeson.ToJSON)

path3 :: Route.Spec (Category, ProductID)
path3 = do
  Route.static "product"
  category <- Route.param @Category "category"
  productID <- Route.param @ProductID "productID"
  pure (category, productID)

testPlan =
  Plan
    id
    ( Operation
        HTTP.GET
        do
          Route.static "index"
          magicNumber <- Route.param @Int "magicNumber"
          pure magicNumber
        do
          x <- Query.param @Int "x"
          y <- Query.option 10 $ Query.param @Int "y"
          pure (x, y)
        do pure ()
        do pure ()
        do
          itsOk <- Response.json @Int HTTP.status200 do
            addSecretNumber <- AddHeaders.using @Int "X-SECRET"
            pure addSecretNumber
          pure itsOk
    )
    \magicNumber (x, y) () () responder ->
      do
        let newNumber = magicNumber + x * y
        print newNumber
        return $ responder (\addHeader response -> addHeader (newNumber * 100) response) newNumber
