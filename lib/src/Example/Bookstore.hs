{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use if" #-}

module Example.Bookstore where

import Control.Natural qualified as Natural
import Data.Aeson qualified as Aeson
import Data.Binary.Builder qualified as Builder
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Lazy.Char8 qualified as LBSChar8
import Data.Kind
import Data.List qualified as List
import Data.Text qualified as Text
import Data.Type.Equality qualified as Equality
import Data.Typeable qualified as Typeable
import Data.Vault.Lazy qualified as Vault
import GHC.Exts qualified as Exts
import GHC.Generics qualified as Generics
import GHC.Natural qualified as Natural
import Network.HTTP.Types qualified as HTTP
import Network.Wai qualified as Wai
import Network.Wai.Handler.Warp qualified as Warp
import Okapi.App {- qualified as App -}
import Okapi.Response {- qualified as Response -}
import Web.HttpApiData qualified as Web

-- Data types representing books, authors, genres, and user preferences
data Book = Book
    { bookId :: Int
    , title :: Text.Text
    , authorId :: Int
    , genreId :: Int
    }
    deriving (Generics.Generic, Aeson.ToJSON, Show)
data Author = Author
    { authorId :: Int
    , authorName :: Text.Text
    }
    deriving (Generics.Generic, Aeson.ToJSON, Show)
data Genre = Genre
    { genreId :: Int
    , genreName :: Text.Text
    }
    deriving (Generics.Generic, Aeson.ToJSON, Show)
data UserPreference = UserPreference
    { userId :: Int
    , bookId :: Int
    }
    deriving (Generics.Generic, Aeson.ToJSON, Show)

-- API for listing books, authors, and genres
bookstoreApi =
    choice
        [ lit "books"
            $ choice
                [ lit "list"
                    . responder @200 @'[] @Aeson.Value @[Book]
                    . method HTTP.GET id
                    $ \ok _req ->
                        return $ ok noHeaders [Book 1 "The Hobbit" 1 1, Book 2 "1984" 2 2]
                , lit "details"
                    . param @Int
                    . responder @200 @'[] @Aeson.Value @Book
                    . responder @500 @'[] @Aeson.Value @Text.Text
                    . method HTTP.GET id
                    $ \bookId ok bookNotFound _req ->
                        return $ case findBook bookId of
                            Just book -> ok noHeaders book
                            Nothing -> bookNotFound noHeaders "Book not found"
                ]
        , lit "authors"
            . responder @200 @'[] @Aeson.Value @[Author]
            . method HTTP.GET id
            $ \ok _req ->
                return $ ok noHeaders [Author 1 "J.R.R. Tolkien", Author 2 "George Orwell"]
        , lit "genres"
            . responder @200 @'[] @Aeson.Value @[Genre]
            . method HTTP.GET id
            $ \ok _req ->
                return $ ok noHeaders [Genre 1 "Fantasy", Genre 2 "Dystopian"]
        ]

-- API for user preferences
userApi =
    lit "user"
        $ choice
            [ lit "preferences"
                -- . authenticateUser -- Middleware for user authentication
                . param @Int
                . responder @200 @'[] @Aeson.Value @[Book]
                . responder @500 @'[] @Text.Text @Text.Text
                . method HTTP.GET id
                $ \userId ok userNotFound _req ->
                    return $ case getUserPreferences userId of
                        Just preferences -> ok noHeaders preferences
                        Nothing -> userNotFound noHeaders "User not found"
            ]

-- Combining the Bookstore and User APIs
api = choice [bookstoreApi, userApi]

-- Helper function to find a book by ID (replace with database query)
findBook :: Int -> Maybe Book
findBook 1 = Just $ Book 1 "The Hobbit" 1 1
findBook 2 = Just $ Book 2 "1984" 2 2
findBook _ = Nothing

-- Helper function to get user preferences (replace with database query)
getUserPreferences :: Int -> Maybe [Book]
getUserPreferences userId
    | userId == 1 = Just [Book 1 "The Hobbit" 1 1]
    | userId == 2 = Just [Book 2 "1984" 2 2]
    | otherwise = Nothing

-- Run the API on port 8009
main :: IO ()
main = Warp.run 8009 . withDefault api $ \req resp ->
    resp $ Wai.responseLBS HTTP.status404 [] "Not Found..."