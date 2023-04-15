{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

import Control.Exception (evaluate)
import qualified Data.Text as Text
import qualified Okapi.Endpoint.Path as Path
import qualified Okapi.Endpoint.Query as Query
import Test.Hspec
import qualified Web.HttpApiData as Web

main :: IO ()
main = hspec $ do
  describe "Path Operations" $ do
    it "returns the first element of a list" $ do
      Path.run path1 ["index"] `shouldBe` (Right (), [])

    it "returns the first element of a list" $ do
      Path.run path1 ["index", "about"] `shouldBe` (Left Path.NotEnoughOperations, ["index", "about"])

    it "returns the first element of a list" $ do
      Path.run path2 ["item", "5"] `shouldBe` (Right 5, [])

    it "returns the first element of a list" $ do
      Path.run path2 ["item"] `shouldBe` (Left Path.TooManyOperations, ["item"])

    it "returns the first element of a list" $ do
      Path.run path3 ["product", "books", "56708"] `shouldBe` (Right (Category "books", ProductID 56708), [])

    it "returns the first element of a list" $ do
      Path.run path3 ["product", "books", "56708", "info"] `shouldBe` (Left Path.NotEnoughOperations, ["product", "books", "56708", "info"])

    it "returns the first element of a list" $ do
      Path.run path3 ["product", "books"] `shouldBe` (Left Path.TooManyOperations, ["product", "books"])

    it "returns the first element of a list" $ do
      Path.run path3' ["product", "books", "56708"] `shouldBe` (Right (Category "books", ProductID 56708), [])

    it "returns the first element of a list" $ do
      Path.run path3' ["product", "books", "56708", "info"] `shouldBe` (Left Path.NotEnoughOperations, ["product", "books", "56708", "info"])

    it "returns the first element of a list" $ do
      Path.run path3' ["product", "books"] `shouldBe` (Left Path.TooManyOperations, ["product", "books"])

  describe "Query Operations" $ do
    it "returns the first element of a list" $ do
      Query.run query1 [("score", Just "5"), ("user", Just "Joe")] `shouldBe` (Right $ Filter 5 $ Username "Joe", [])

    it "returns the first element of a list" $ do
      Query.run query2 [("user", Just "Bob"), ("active", Nothing)] `shouldBe` (Right $ Username "Bob", [])

    it "returns the first element of a list" $ do
      Query.run query3 [("username", Just "Bob")] `shouldBe` (Right (Username "Anon", Nothing), [("username", Just "Bob")])

newtype Username = Username {unwrap :: Text.Text}
  deriving (Eq, Show, Web.FromHttpApiData)

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
