{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.ByteString.Lazy qualified as LBS
import Data.Function ((&))
import Data.Kind (Type)
import GHC.Generics (Generic)
import Network.HTTP.Types qualified as HTTP
import Okapi
import Okapi.HTTP.Response.Status (S200, S201, S204)
import Okapi.Mode.Data qualified as Data

-- 6 distinct Shape types covering all 8 route patterns
type ListRoute    = Shape GET    ()  HTTP.Query HTTP.RequestHeaders (IO LBS.ByteString) (Data.Response S200 HTTP.ResponseHeaders (IO LBS.ByteString))
type GetRoute     = Shape GET    Int HTTP.Query HTTP.RequestHeaders (IO LBS.ByteString) (Data.Response S200 HTTP.ResponseHeaders (IO LBS.ByteString))
type PostRoute    = Shape POST   ()  HTTP.Query HTTP.RequestHeaders (IO LBS.ByteString) (Data.Response S201 HTTP.ResponseHeaders (IO LBS.ByteString))
type PutRoute     = Shape PUT    Int HTTP.Query HTTP.RequestHeaders (IO LBS.ByteString) (Data.Response S200 HTTP.ResponseHeaders (IO LBS.ByteString))
type DeleteRoute  = Shape DELETE Int HTTP.Query HTTP.RequestHeaders (IO LBS.ByteString) (Data.Response S204 HTTP.ResponseHeaders (IO LBS.ByteString))
type PostIntRoute = Shape POST   Int HTTP.Query HTTP.RequestHeaders (IO LBS.ByteString) (Data.Response S201 HTTP.ResponseHeaders (IO LBS.ByteString))

-- 128 routes: 16 groups x 8 patterns each
-- patterns: list, get, create, update, delete, sub-list, sub-create, search
data Api (f :: Type -> Type) = Api
    -- group 0: users
    { users_list       :: f ListRoute
    , users_get        :: f GetRoute
    , users_create     :: f PostRoute
    , users_update     :: f PutRoute
    , users_delete     :: f DeleteRoute
    , users_sub_list   :: f GetRoute
    , users_sub_create :: f PostIntRoute
    , users_search     :: f ListRoute
    -- group 1: posts
    , posts_list       :: f ListRoute
    , posts_get        :: f GetRoute
    , posts_create     :: f PostRoute
    , posts_update     :: f PutRoute
    , posts_delete     :: f DeleteRoute
    , posts_sub_list   :: f GetRoute
    , posts_sub_create :: f PostIntRoute
    , posts_search     :: f ListRoute
    -- group 2: comments
    , comments_list       :: f ListRoute
    , comments_get        :: f GetRoute
    , comments_create     :: f PostRoute
    , comments_update     :: f PutRoute
    , comments_delete     :: f DeleteRoute
    , comments_sub_list   :: f GetRoute
    , comments_sub_create :: f PostIntRoute
    , comments_search     :: f ListRoute
    -- group 3: tags
    , tags_list       :: f ListRoute
    , tags_get        :: f GetRoute
    , tags_create     :: f PostRoute
    , tags_update     :: f PutRoute
    , tags_delete     :: f DeleteRoute
    , tags_sub_list   :: f GetRoute
    , tags_sub_create :: f PostIntRoute
    , tags_search     :: f ListRoute
    -- group 4: categories
    , categories_list       :: f ListRoute
    , categories_get        :: f GetRoute
    , categories_create     :: f PostRoute
    , categories_update     :: f PutRoute
    , categories_delete     :: f DeleteRoute
    , categories_sub_list   :: f GetRoute
    , categories_sub_create :: f PostIntRoute
    , categories_search     :: f ListRoute
    -- group 5: products
    , products_list       :: f ListRoute
    , products_get        :: f GetRoute
    , products_create     :: f PostRoute
    , products_update     :: f PutRoute
    , products_delete     :: f DeleteRoute
    , products_sub_list   :: f GetRoute
    , products_sub_create :: f PostIntRoute
    , products_search     :: f ListRoute
    -- group 6: orders
    , orders_list       :: f ListRoute
    , orders_get        :: f GetRoute
    , orders_create     :: f PostRoute
    , orders_update     :: f PutRoute
    , orders_delete     :: f DeleteRoute
    , orders_sub_list   :: f GetRoute
    , orders_sub_create :: f PostIntRoute
    , orders_search     :: f ListRoute
    -- group 7: reviews
    , reviews_list       :: f ListRoute
    , reviews_get        :: f GetRoute
    , reviews_create     :: f PostRoute
    , reviews_update     :: f PutRoute
    , reviews_delete     :: f DeleteRoute
    , reviews_sub_list   :: f GetRoute
    , reviews_sub_create :: f PostIntRoute
    , reviews_search     :: f ListRoute
    -- group 8: articles
    , articles_list       :: f ListRoute
    , articles_get        :: f GetRoute
    , articles_create     :: f PostRoute
    , articles_update     :: f PutRoute
    , articles_delete     :: f DeleteRoute
    , articles_sub_list   :: f GetRoute
    , articles_sub_create :: f PostIntRoute
    , articles_search     :: f ListRoute
    -- group 9: albums
    , albums_list       :: f ListRoute
    , albums_get        :: f GetRoute
    , albums_create     :: f PostRoute
    , albums_update     :: f PutRoute
    , albums_delete     :: f DeleteRoute
    , albums_sub_list   :: f GetRoute
    , albums_sub_create :: f PostIntRoute
    , albums_search     :: f ListRoute
    -- group 10: tracks
    , tracks_list       :: f ListRoute
    , tracks_get        :: f GetRoute
    , tracks_create     :: f PostRoute
    , tracks_update     :: f PutRoute
    , tracks_delete     :: f DeleteRoute
    , tracks_sub_list   :: f GetRoute
    , tracks_sub_create :: f PostIntRoute
    , tracks_search     :: f ListRoute
    -- group 11: playlists
    , playlists_list       :: f ListRoute
    , playlists_get        :: f GetRoute
    , playlists_create     :: f PostRoute
    , playlists_update     :: f PutRoute
    , playlists_delete     :: f DeleteRoute
    , playlists_sub_list   :: f GetRoute
    , playlists_sub_create :: f PostIntRoute
    , playlists_search     :: f ListRoute
    -- group 12: events
    , events_list       :: f ListRoute
    , events_get        :: f GetRoute
    , events_create     :: f PostRoute
    , events_update     :: f PutRoute
    , events_delete     :: f DeleteRoute
    , events_sub_list   :: f GetRoute
    , events_sub_create :: f PostIntRoute
    , events_search     :: f ListRoute
    -- group 13: venues
    , venues_list       :: f ListRoute
    , venues_get        :: f GetRoute
    , venues_create     :: f PostRoute
    , venues_update     :: f PutRoute
    , venues_delete     :: f DeleteRoute
    , venues_sub_list   :: f GetRoute
    , venues_sub_create :: f PostIntRoute
    , venues_search     :: f ListRoute
    -- group 14: artists
    , artists_list       :: f ListRoute
    , artists_get        :: f GetRoute
    , artists_create     :: f PostRoute
    , artists_update     :: f PutRoute
    , artists_delete     :: f DeleteRoute
    , artists_sub_list   :: f GetRoute
    , artists_sub_create :: f PostIntRoute
    , artists_search     :: f ListRoute
    -- group 15: genres
    , genres_list       :: f ListRoute
    , genres_get        :: f GetRoute
    , genres_create     :: f PostRoute
    , genres_update     :: f PutRoute
    , genres_delete     :: f DeleteRoute
    , genres_sub_list   :: f GetRoute
    , genres_sub_create :: f PostIntRoute
    , genres_search     :: f ListRoute
    } deriving Generic

list_   :: Canopy ListRoute
list_    = (requestGET  & path (segment_ text "r")) :-> response200

get_    :: Canopy GetRoute
get_     = (requestGET & path (segment "id" int)) :-> response200

create_ :: Canopy PostRoute
create_  = (requestPOST & path (segment_ text "r")) :-> response201

update_ :: Canopy PutRoute
update_  = (requestPUT & path (segment "id" int)) :-> response200

delete_ :: Canopy DeleteRoute
delete_  = (requestDELETE & path (segment "id" int)) :-> response204

subCreate_ :: Canopy PostIntRoute
subCreate_ = (requestPOST & path (segment "id" int)) :-> response201

api :: Api Canopy
api = Api
    { users_list = list_, users_get = get_, users_create = create_
    , users_update = update_, users_delete = delete_
    , users_sub_list = get_, users_sub_create = subCreate_, users_search = list_
    , posts_list = list_, posts_get = get_, posts_create = create_
    , posts_update = update_, posts_delete = delete_
    , posts_sub_list = get_, posts_sub_create = subCreate_, posts_search = list_
    , comments_list = list_, comments_get = get_, comments_create = create_
    , comments_update = update_, comments_delete = delete_
    , comments_sub_list = get_, comments_sub_create = subCreate_, comments_search = list_
    , tags_list = list_, tags_get = get_, tags_create = create_
    , tags_update = update_, tags_delete = delete_
    , tags_sub_list = get_, tags_sub_create = subCreate_, tags_search = list_
    , categories_list = list_, categories_get = get_, categories_create = create_
    , categories_update = update_, categories_delete = delete_
    , categories_sub_list = get_, categories_sub_create = subCreate_, categories_search = list_
    , products_list = list_, products_get = get_, products_create = create_
    , products_update = update_, products_delete = delete_
    , products_sub_list = get_, products_sub_create = subCreate_, products_search = list_
    , orders_list = list_, orders_get = get_, orders_create = create_
    , orders_update = update_, orders_delete = delete_
    , orders_sub_list = get_, orders_sub_create = subCreate_, orders_search = list_
    , reviews_list = list_, reviews_get = get_, reviews_create = create_
    , reviews_update = update_, reviews_delete = delete_
    , reviews_sub_list = get_, reviews_sub_create = subCreate_, reviews_search = list_
    , articles_list = list_, articles_get = get_, articles_create = create_
    , articles_update = update_, articles_delete = delete_
    , articles_sub_list = get_, articles_sub_create = subCreate_, articles_search = list_
    , albums_list = list_, albums_get = get_, albums_create = create_
    , albums_update = update_, albums_delete = delete_
    , albums_sub_list = get_, albums_sub_create = subCreate_, albums_search = list_
    , tracks_list = list_, tracks_get = get_, tracks_create = create_
    , tracks_update = update_, tracks_delete = delete_
    , tracks_sub_list = get_, tracks_sub_create = subCreate_, tracks_search = list_
    , playlists_list = list_, playlists_get = get_, playlists_create = create_
    , playlists_update = update_, playlists_delete = delete_
    , playlists_sub_list = get_, playlists_sub_create = subCreate_, playlists_search = list_
    , events_list = list_, events_get = get_, events_create = create_
    , events_update = update_, events_delete = delete_
    , events_sub_list = get_, events_sub_create = subCreate_, events_search = list_
    , venues_list = list_, venues_get = get_, venues_create = create_
    , venues_update = update_, venues_delete = delete_
    , venues_sub_list = get_, venues_sub_create = subCreate_, venues_search = list_
    , artists_list = list_, artists_get = get_, artists_create = create_
    , artists_update = update_, artists_delete = delete_
    , artists_sub_list = get_, artists_sub_create = subCreate_, artists_search = list_
    , genres_list = list_, genres_get = get_, genres_create = create_
    , genres_update = update_, genres_delete = delete_
    , genres_sub_list = get_, genres_sub_create = subCreate_, genres_search = list_
    }

main :: IO ()
main = putStrLn "OK"



-- bench

-- bench3
