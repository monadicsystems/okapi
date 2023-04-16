{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

import Control.Exception (evaluate)
import qualified Data.Aeson as Aeson
import qualified Data.Text as Text
import qualified Network.HTTP.Types as HTTP
import qualified Okapi.Endpoint.Path as Path
import qualified Okapi.Endpoint.Query as Query
import qualified Okapi.Endpoint.Responder as Responder
import qualified Okapi.Endpoint.ResponderHeaders as ResponderHeaders
import qualified Prelude.Linear as Linear
import Test.Hspec
import qualified Web.HttpApiData as Web

main :: IO ()
main = hspec $ do
  describe "Path Operations" $ do
    it "returns the first element of a list" $ do
      Path.eval path1 ["index"] `shouldBe` (Right (), [])

    it "returns the first element of a list" $ do
      Path.eval path1 ["index", "about"] `shouldBe` (Left Path.NotEnoughOperations, ["index", "about"])

    it "returns the first element of a list" $ do
      Path.eval path2 ["item", "5"] `shouldBe` (Right 5, [])

    it "returns the first element of a list" $ do
      Path.eval path2 ["item"] `shouldBe` (Left Path.TooManyOperations, ["item"])

    it "returns the first element of a list" $ do
      Path.eval path3 ["product", "books", "56708"] `shouldBe` (Right (Category "books", ProductID 56708), [])

    it "returns the first element of a list" $ do
      Path.eval path3 ["product", "books", "56708", "info"] `shouldBe` (Left Path.NotEnoughOperations, ["product", "books", "56708", "info"])

    it "returns the first element of a list" $ do
      Path.eval path3 ["product", "books"] `shouldBe` (Left Path.TooManyOperations, ["product", "books"])

    it "returns the first element of a list" $ do
      Path.eval path3' ["product", "books", "56708"] `shouldBe` (Right (Category "books", ProductID 56708), [])

    it "returns the first element of a list" $ do
      Path.eval path3' ["product", "books", "56708", "info"] `shouldBe` (Left Path.NotEnoughOperations, ["product", "books", "56708", "info"])

    it "returns the first element of a list" $ do
      Path.eval path3' ["product", "books"] `shouldBe` (Left Path.TooManyOperations, ["product", "books"])

  describe "Query Operations" $ do
    it "returns the first element of a list" $ do
      Query.eval query1 [("score", Just "5"), ("user", Just "Joe")] `shouldBe` (Right $ Filter 5 $ Username "Joe", [])

    it "returns the first element of a list" $ do
      Query.eval query2 [("user", Just "Bob"), ("active", Nothing)] `shouldBe` (Right $ Username "Bob", [])

    it "returns the first element of a list" $ do
      Query.eval query3 [("username", Just "Bob")] `shouldBe` (Right (Username "Anon", Nothing), [("username", Just "Bob")])

{-
  describe "Responder Operations" $ do
    it "returns the first element of a list" $ do
      Query.eval responder1 [("score", Just "5"), ("user", Just "Joe")] `shouldBe` (Right $ Filter 5 $ Username "Joe", [])

    it "returns the first element of a list" $ do
      Query.eval responder2 [("user", Just "Bob"), ("active", Nothing)] `shouldBe` (Right $ Username "Bob", [])

    it "returns the first element of a list" $ do
      Query.eval responder2 [("username", Just "Bob")] `shouldBe` (Right (Username "Anon", Nothing), [("username", Just "Bob")])
-}

data AddHeaders = AddHeaders
  { addCookie :: Username -> ResponderHeaders.Response -> ResponderHeaders.Response,
    addAnotherHeader :: Username -> ResponderHeaders.Response -> ResponderHeaders.Response,
    cacheHeader :: Int -> ResponderHeaders.Response -> ResponderHeaders.Response
  }

responder1 ::
  Responder.Responder
    ( ( ( Username ->
          ResponderHeaders.Response ->
          ResponderHeaders.Response
        ) %1 ->
        ResponderHeaders.Response ->
        ResponderHeaders.Response
      ) ->
      Aeson.Value ->
      ResponderHeaders.Response,
      ( AddHeaders %1 ->
        ResponderHeaders.Response ->
        ResponderHeaders.Response
      ) ->
      Aeson.Value ->
      ResponderHeaders.Response
    )
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

(%.) :: (a -> b) %1 -> (b -> c) %1 -> (a -> c)
(%.) f g x = g $ f x

responder2 = undefined

newtype Username = Username {unwrap :: Text.Text}
  deriving (Eq, Show, Web.FromHttpApiData, Web.ToHttpApiData)

data Filter = Filter
  { score :: Int,
    byUser :: Username
  }
  deriving (Eq, Show)

query1 :: Query.Query Filter
query1 = do
  score <- Query.param @Int "score"
  byUser <- Query.param @Username "user"
  pure Filter {..}

query2 :: Query.Query Username
query2 = do
  username <- Query.param @Username "user"
  Query.flag "active"
  pure username

query3 :: Query.Query (Username, Maybe ())
query3 = do
  user <- Query.option (Username "Anon") $ Query.param "user"
  active <- Query.optional $ Query.flag "active"
  pure (user, active)

path1 :: Path.Path ()
path1 = Path.static "index"

path2 :: Path.Path Int
path2 = do
  Path.static "item"
  uuid <- Path.param @Int
  pure uuid

newtype Category = Category {unwrap :: Text.Text}
  deriving (Eq, Show, Web.FromHttpApiData)

newtype ProductID = ProductID {unwrap :: Int}
  deriving (Eq, Show, Web.FromHttpApiData)

path3 :: Path.Path (Category, ProductID)
path3 = Path.static "product" *> ((,) <$> Path.param @Category <*> Path.param @ProductID)

path3' :: Path.Path (Category, ProductID)
path3' = do
  Path.static "product"
  category <- Path.param @Category
  productID <- Path.param @ProductID
  pure (category, productID)
