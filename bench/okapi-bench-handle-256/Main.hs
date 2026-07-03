{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.ByteString.Lazy qualified as LBS
import Data.Function ((&))
import Network.HTTP.Types qualified as HTTP
import Okapi
import Okapi.HTTP.Response.Status (S200, S201, S204)
import Okapi.Mode.Data qualified as Data

type ListRoute    = Shape GET    ()  HTTP.Query HTTP.RequestHeaders (IO LBS.ByteString) (Data.Response S200 HTTP.ResponseHeaders (IO LBS.ByteString))
type GetRoute     = Shape GET    Int HTTP.Query HTTP.RequestHeaders (IO LBS.ByteString) (Data.Response S200 HTTP.ResponseHeaders (IO LBS.ByteString))
type PostRoute    = Shape POST   ()  HTTP.Query HTTP.RequestHeaders (IO LBS.ByteString) (Data.Response S201 HTTP.ResponseHeaders (IO LBS.ByteString))
type PutRoute     = Shape PUT    Int HTTP.Query HTTP.RequestHeaders (IO LBS.ByteString) (Data.Response S200 HTTP.ResponseHeaders (IO LBS.ByteString))
type DeleteRoute  = Shape DELETE Int HTTP.Query HTTP.RequestHeaders (IO LBS.ByteString) (Data.Response S204 HTTP.ResponseHeaders (IO LBS.ByteString))
type PostIntRoute = Shape POST   Int HTTP.Query HTTP.RequestHeaders (IO LBS.ByteString) (Data.Response S201 HTTP.ResponseHeaders (IO LBS.ByteString))

list_      :: Canopy ListRoute
list_       = (methodGET  & path (segment_ text "r")) :-> status200

get_       :: Canopy GetRoute
get_        = (methodGET & path (segment "id" int)) :-> status200

create_    :: Canopy PostRoute
create_     = (methodPOST & path (segment_ text "r")) :-> status201

update_    :: Canopy PutRoute
update_     = (methodPUT & path (segment "id" int)) :-> status200

delete_    :: Canopy DeleteRoute
delete_     = (methodDELETE & path (segment "id" int)) :-> status204

subCreate_ :: Canopy PostIntRoute
subCreate_  = (methodPOST & path (segment "id" int)) :-> status201

listH, getH, postH, putH, deleteH, postIntH :: Handle
listH    = handle id (list_      := fn \_ -> undefined)
getH     = handle id (get_       := fn \_ -> undefined)
postH    = handle id (create_    := fn \_ -> undefined)
putH     = handle id (update_    := fn \_ -> undefined)
deleteH  = handle id (delete_    := fn \_ -> undefined)
postIntH = handle id (subCreate_ := fn \_ -> undefined)

-- 256 routes: 32 groups x 8 patterns each
routes :: [Handle]
routes =
    -- group 0: users
    [ listH, getH, postH, putH, deleteH, getH, postIntH, listH
    -- group 1: posts
    , listH, getH, postH, putH, deleteH, getH, postIntH, listH
    -- group 2: comments
    , listH, getH, postH, putH, deleteH, getH, postIntH, listH
    -- group 3: tags
    , listH, getH, postH, putH, deleteH, getH, postIntH, listH
    -- group 4: categories
    , listH, getH, postH, putH, deleteH, getH, postIntH, listH
    -- group 5: products
    , listH, getH, postH, putH, deleteH, getH, postIntH, listH
    -- group 6: orders
    , listH, getH, postH, putH, deleteH, getH, postIntH, listH
    -- group 7: reviews
    , listH, getH, postH, putH, deleteH, getH, postIntH, listH
    -- group 8: articles
    , listH, getH, postH, putH, deleteH, getH, postIntH, listH
    -- group 9: albums
    , listH, getH, postH, putH, deleteH, getH, postIntH, listH
    -- group 10: tracks
    , listH, getH, postH, putH, deleteH, getH, postIntH, listH
    -- group 11: playlists
    , listH, getH, postH, putH, deleteH, getH, postIntH, listH
    -- group 12: events
    , listH, getH, postH, putH, deleteH, getH, postIntH, listH
    -- group 13: venues
    , listH, getH, postH, putH, deleteH, getH, postIntH, listH
    -- group 14: artists
    , listH, getH, postH, putH, deleteH, getH, postIntH, listH
    -- group 15: genres
    , listH, getH, postH, putH, deleteH, getH, postIntH, listH
    -- group 16: users2
    , listH, getH, postH, putH, deleteH, getH, postIntH, listH
    -- group 17: posts2
    , listH, getH, postH, putH, deleteH, getH, postIntH, listH
    -- group 18: comments2
    , listH, getH, postH, putH, deleteH, getH, postIntH, listH
    -- group 19: tags2
    , listH, getH, postH, putH, deleteH, getH, postIntH, listH
    -- group 20: categories2
    , listH, getH, postH, putH, deleteH, getH, postIntH, listH
    -- group 21: products2
    , listH, getH, postH, putH, deleteH, getH, postIntH, listH
    -- group 22: orders2
    , listH, getH, postH, putH, deleteH, getH, postIntH, listH
    -- group 23: reviews2
    , listH, getH, postH, putH, deleteH, getH, postIntH, listH
    -- group 24: articles2
    , listH, getH, postH, putH, deleteH, getH, postIntH, listH
    -- group 25: albums2
    , listH, getH, postH, putH, deleteH, getH, postIntH, listH
    -- group 26: tracks2
    , listH, getH, postH, putH, deleteH, getH, postIntH, listH
    -- group 27: playlists2
    , listH, getH, postH, putH, deleteH, getH, postIntH, listH
    -- group 28: events2
    , listH, getH, postH, putH, deleteH, getH, postIntH, listH
    -- group 29: venues2
    , listH, getH, postH, putH, deleteH, getH, postIntH, listH
    -- group 30: artists2
    , listH, getH, postH, putH, deleteH, getH, postIntH, listH
    -- group 31: genres2
    , listH, getH, postH, putH, deleteH, getH, postIntH, listH
    ]

main :: IO ()
main = putStrLn "OK"

-- 1

-- 2

-- 3
