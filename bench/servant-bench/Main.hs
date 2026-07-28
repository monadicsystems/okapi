{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Aeson (ToJSON, FromJSON)
import Data.Text (Text)
import GHC.Generics (Generic)
import Servant

-- 16 resource types, one per route group
data User     = User     Int Text deriving Generic
data BlogPost = BlogPost Int Text deriving Generic
data Comment  = Comment  Int Text deriving Generic
data Tag      = Tag      Int Text deriving Generic
data Category = Category Int Text deriving Generic
data Product  = Product  Int Text deriving Generic
data Order    = Order    Int Int  deriving Generic
data Review   = Review   Int Int  deriving Generic
data Article  = Article  Int Text deriving Generic
data Album    = Album    Int Text deriving Generic
data Track    = Track    Int Text deriving Generic
data Playlist = Playlist Int Text deriving Generic
data Event    = Event    Int Text deriving Generic
data Venue    = Venue    Int Text deriving Generic
data Artist   = Artist   Int Text deriving Generic
data Genre    = Genre    Int Text deriving Generic

instance ToJSON User;     instance FromJSON User
instance ToJSON BlogPost; instance FromJSON BlogPost
instance ToJSON Comment;  instance FromJSON Comment
instance ToJSON Tag;      instance FromJSON Tag
instance ToJSON Category; instance FromJSON Category
instance ToJSON Product;  instance FromJSON Product
instance ToJSON Order;    instance FromJSON Order
instance ToJSON Review;   instance FromJSON Review
instance ToJSON Article;  instance FromJSON Article
instance ToJSON Album;    instance FromJSON Album
instance ToJSON Track;    instance FromJSON Track
instance ToJSON Playlist; instance FromJSON Playlist
instance ToJSON Event;    instance FromJSON Event
instance ToJSON Venue;    instance FromJSON Venue
instance ToJSON Artist;   instance FromJSON Artist
instance ToJSON Genre;    instance FromJSON Genre

-- 128 routes: 16 groups x 8 routes each
-- patterns: list, get, create, update, delete, sub-list, sub-create, search
type API
    -- group 0: users
    =    "users" :> Get '[JSON] [User]
    :<|> "users" :> Capture "id" Int :> Get '[JSON] User
    :<|> "users" :> ReqBody '[JSON] User :> Post '[JSON] User
    :<|> "users" :> Capture "id" Int :> ReqBody '[JSON] User :> Put '[JSON] User
    :<|> "users" :> Capture "id" Int :> Delete '[JSON] NoContent
    :<|> "users" :> Capture "id" Int :> "items" :> Get '[JSON] [User]
    :<|> "users" :> Capture "id" Int :> "items" :> ReqBody '[JSON] User :> Post '[JSON] User
    :<|> "users" :> "search" :> QueryParam "q" Text :> Get '[JSON] [User]
    -- group 1: posts
    :<|> "posts" :> Get '[JSON] [BlogPost]
    :<|> "posts" :> Capture "id" Int :> Get '[JSON] BlogPost
    :<|> "posts" :> ReqBody '[JSON] BlogPost :> Post '[JSON] BlogPost
    :<|> "posts" :> Capture "id" Int :> ReqBody '[JSON] BlogPost :> Put '[JSON] BlogPost
    :<|> "posts" :> Capture "id" Int :> Delete '[JSON] NoContent
    :<|> "posts" :> Capture "id" Int :> "comments" :> Get '[JSON] [BlogPost]
    :<|> "posts" :> Capture "id" Int :> "comments" :> ReqBody '[JSON] BlogPost :> Post '[JSON] BlogPost
    :<|> "posts" :> "search" :> QueryParam "q" Text :> Get '[JSON] [BlogPost]
    -- group 2: comments
    :<|> "comments" :> Get '[JSON] [Comment]
    :<|> "comments" :> Capture "id" Int :> Get '[JSON] Comment
    :<|> "comments" :> ReqBody '[JSON] Comment :> Post '[JSON] Comment
    :<|> "comments" :> Capture "id" Int :> ReqBody '[JSON] Comment :> Put '[JSON] Comment
    :<|> "comments" :> Capture "id" Int :> Delete '[JSON] NoContent
    :<|> "comments" :> Capture "id" Int :> "replies" :> Get '[JSON] [Comment]
    :<|> "comments" :> Capture "id" Int :> "replies" :> ReqBody '[JSON] Comment :> Post '[JSON] Comment
    :<|> "comments" :> "search" :> QueryParam "q" Text :> Get '[JSON] [Comment]
    -- group 3: tags
    :<|> "tags" :> Get '[JSON] [Tag]
    :<|> "tags" :> Capture "id" Int :> Get '[JSON] Tag
    :<|> "tags" :> ReqBody '[JSON] Tag :> Post '[JSON] Tag
    :<|> "tags" :> Capture "id" Int :> ReqBody '[JSON] Tag :> Put '[JSON] Tag
    :<|> "tags" :> Capture "id" Int :> Delete '[JSON] NoContent
    :<|> "tags" :> Capture "id" Int :> "children" :> Get '[JSON] [Tag]
    :<|> "tags" :> Capture "id" Int :> "children" :> ReqBody '[JSON] Tag :> Post '[JSON] Tag
    :<|> "tags" :> "search" :> QueryParam "q" Text :> Get '[JSON] [Tag]
    -- group 4: categories
    :<|> "categories" :> Get '[JSON] [Category]
    :<|> "categories" :> Capture "id" Int :> Get '[JSON] Category
    :<|> "categories" :> ReqBody '[JSON] Category :> Post '[JSON] Category
    :<|> "categories" :> Capture "id" Int :> ReqBody '[JSON] Category :> Put '[JSON] Category
    :<|> "categories" :> Capture "id" Int :> Delete '[JSON] NoContent
    :<|> "categories" :> Capture "id" Int :> "sub" :> Get '[JSON] [Category]
    :<|> "categories" :> Capture "id" Int :> "sub" :> ReqBody '[JSON] Category :> Post '[JSON] Category
    :<|> "categories" :> "search" :> QueryParam "q" Text :> Get '[JSON] [Category]
    -- group 5: products
    :<|> "products" :> Get '[JSON] [Product]
    :<|> "products" :> Capture "id" Int :> Get '[JSON] Product
    :<|> "products" :> ReqBody '[JSON] Product :> Post '[JSON] Product
    :<|> "products" :> Capture "id" Int :> ReqBody '[JSON] Product :> Put '[JSON] Product
    :<|> "products" :> Capture "id" Int :> Delete '[JSON] NoContent
    :<|> "products" :> Capture "id" Int :> "variants" :> Get '[JSON] [Product]
    :<|> "products" :> Capture "id" Int :> "variants" :> ReqBody '[JSON] Product :> Post '[JSON] Product
    :<|> "products" :> "search" :> QueryParam "q" Text :> Get '[JSON] [Product]
    -- group 6: orders
    :<|> "orders" :> Get '[JSON] [Order]
    :<|> "orders" :> Capture "id" Int :> Get '[JSON] Order
    :<|> "orders" :> ReqBody '[JSON] Order :> Post '[JSON] Order
    :<|> "orders" :> Capture "id" Int :> ReqBody '[JSON] Order :> Put '[JSON] Order
    :<|> "orders" :> Capture "id" Int :> Delete '[JSON] NoContent
    :<|> "orders" :> Capture "id" Int :> "items" :> Get '[JSON] [Order]
    :<|> "orders" :> Capture "id" Int :> "items" :> ReqBody '[JSON] Order :> Post '[JSON] Order
    :<|> "orders" :> "search" :> QueryParam "q" Text :> Get '[JSON] [Order]
    -- group 7: reviews
    :<|> "reviews" :> Get '[JSON] [Review]
    :<|> "reviews" :> Capture "id" Int :> Get '[JSON] Review
    :<|> "reviews" :> ReqBody '[JSON] Review :> Post '[JSON] Review
    :<|> "reviews" :> Capture "id" Int :> ReqBody '[JSON] Review :> Put '[JSON] Review
    :<|> "reviews" :> Capture "id" Int :> Delete '[JSON] NoContent
    :<|> "reviews" :> Capture "id" Int :> "responses" :> Get '[JSON] [Review]
    :<|> "reviews" :> Capture "id" Int :> "responses" :> ReqBody '[JSON] Review :> Post '[JSON] Review
    :<|> "reviews" :> "search" :> QueryParam "q" Text :> Get '[JSON] [Review]
    -- group 8: articles
    :<|> "articles" :> Get '[JSON] [Article]
    :<|> "articles" :> Capture "id" Int :> Get '[JSON] Article
    :<|> "articles" :> ReqBody '[JSON] Article :> Post '[JSON] Article
    :<|> "articles" :> Capture "id" Int :> ReqBody '[JSON] Article :> Put '[JSON] Article
    :<|> "articles" :> Capture "id" Int :> Delete '[JSON] NoContent
    :<|> "articles" :> Capture "id" Int :> "sections" :> Get '[JSON] [Article]
    :<|> "articles" :> Capture "id" Int :> "sections" :> ReqBody '[JSON] Article :> Post '[JSON] Article
    :<|> "articles" :> "search" :> QueryParam "q" Text :> Get '[JSON] [Article]
    -- group 9: albums
    :<|> "albums" :> Get '[JSON] [Album]
    :<|> "albums" :> Capture "id" Int :> Get '[JSON] Album
    :<|> "albums" :> ReqBody '[JSON] Album :> Post '[JSON] Album
    :<|> "albums" :> Capture "id" Int :> ReqBody '[JSON] Album :> Put '[JSON] Album
    :<|> "albums" :> Capture "id" Int :> Delete '[JSON] NoContent
    :<|> "albums" :> Capture "id" Int :> "tracks" :> Get '[JSON] [Album]
    :<|> "albums" :> Capture "id" Int :> "tracks" :> ReqBody '[JSON] Album :> Post '[JSON] Album
    :<|> "albums" :> "search" :> QueryParam "q" Text :> Get '[JSON] [Album]
    -- group 10: tracks
    :<|> "tracks" :> Get '[JSON] [Track]
    :<|> "tracks" :> Capture "id" Int :> Get '[JSON] Track
    :<|> "tracks" :> ReqBody '[JSON] Track :> Post '[JSON] Track
    :<|> "tracks" :> Capture "id" Int :> ReqBody '[JSON] Track :> Put '[JSON] Track
    :<|> "tracks" :> Capture "id" Int :> Delete '[JSON] NoContent
    :<|> "tracks" :> Capture "id" Int :> "lyrics" :> Get '[JSON] [Track]
    :<|> "tracks" :> Capture "id" Int :> "lyrics" :> ReqBody '[JSON] Track :> Post '[JSON] Track
    :<|> "tracks" :> "search" :> QueryParam "q" Text :> Get '[JSON] [Track]
    -- group 11: playlists
    :<|> "playlists" :> Get '[JSON] [Playlist]
    :<|> "playlists" :> Capture "id" Int :> Get '[JSON] Playlist
    :<|> "playlists" :> ReqBody '[JSON] Playlist :> Post '[JSON] Playlist
    :<|> "playlists" :> Capture "id" Int :> ReqBody '[JSON] Playlist :> Put '[JSON] Playlist
    :<|> "playlists" :> Capture "id" Int :> Delete '[JSON] NoContent
    :<|> "playlists" :> Capture "id" Int :> "tracks" :> Get '[JSON] [Playlist]
    :<|> "playlists" :> Capture "id" Int :> "tracks" :> ReqBody '[JSON] Playlist :> Post '[JSON] Playlist
    :<|> "playlists" :> "search" :> QueryParam "q" Text :> Get '[JSON] [Playlist]
    -- group 12: events
    :<|> "events" :> Get '[JSON] [Event]
    :<|> "events" :> Capture "id" Int :> Get '[JSON] Event
    :<|> "events" :> ReqBody '[JSON] Event :> Post '[JSON] Event
    :<|> "events" :> Capture "id" Int :> ReqBody '[JSON] Event :> Put '[JSON] Event
    :<|> "events" :> Capture "id" Int :> Delete '[JSON] NoContent
    :<|> "events" :> Capture "id" Int :> "attendees" :> Get '[JSON] [Event]
    :<|> "events" :> Capture "id" Int :> "attendees" :> ReqBody '[JSON] Event :> Post '[JSON] Event
    :<|> "events" :> "search" :> QueryParam "q" Text :> Get '[JSON] [Event]
    -- group 13: venues
    :<|> "venues" :> Get '[JSON] [Venue]
    :<|> "venues" :> Capture "id" Int :> Get '[JSON] Venue
    :<|> "venues" :> ReqBody '[JSON] Venue :> Post '[JSON] Venue
    :<|> "venues" :> Capture "id" Int :> ReqBody '[JSON] Venue :> Put '[JSON] Venue
    :<|> "venues" :> Capture "id" Int :> Delete '[JSON] NoContent
    :<|> "venues" :> Capture "id" Int :> "events" :> Get '[JSON] [Venue]
    :<|> "venues" :> Capture "id" Int :> "events" :> ReqBody '[JSON] Venue :> Post '[JSON] Venue
    :<|> "venues" :> "search" :> QueryParam "q" Text :> Get '[JSON] [Venue]
    -- group 14: artists
    :<|> "artists" :> Get '[JSON] [Artist]
    :<|> "artists" :> Capture "id" Int :> Get '[JSON] Artist
    :<|> "artists" :> ReqBody '[JSON] Artist :> Post '[JSON] Artist
    :<|> "artists" :> Capture "id" Int :> ReqBody '[JSON] Artist :> Put '[JSON] Artist
    :<|> "artists" :> Capture "id" Int :> Delete '[JSON] NoContent
    :<|> "artists" :> Capture "id" Int :> "albums" :> Get '[JSON] [Artist]
    :<|> "artists" :> Capture "id" Int :> "albums" :> ReqBody '[JSON] Artist :> Post '[JSON] Artist
    :<|> "artists" :> "search" :> QueryParam "q" Text :> Get '[JSON] [Artist]
    -- group 15: genres
    :<|> "genres" :> Get '[JSON] [Genre]
    :<|> "genres" :> Capture "id" Int :> Get '[JSON] Genre
    :<|> "genres" :> ReqBody '[JSON] Genre :> Post '[JSON] Genre
    :<|> "genres" :> Capture "id" Int :> ReqBody '[JSON] Genre :> Put '[JSON] Genre
    :<|> "genres" :> Capture "id" Int :> Delete '[JSON] NoContent
    :<|> "genres" :> Capture "id" Int :> "artists" :> Get '[JSON] [Genre]
    :<|> "genres" :> Capture "id" Int :> "artists" :> ReqBody '[JSON] Genre :> Post '[JSON] Genre
    :<|> "genres" :> "search" :> QueryParam "q" Text :> Get '[JSON] [Genre]

server :: Server API
server = undefined
main :: IO ()
main = putStrLn "OK"

