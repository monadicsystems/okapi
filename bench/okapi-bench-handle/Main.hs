{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.ByteString.Lazy qualified as LBS
import Data.Function ((&))
import Network.HTTP.Types qualified as HTTP
import Okapi
import Okapi.HTTP.Response.Status (S200, S201, S204)
import Okapi.Mode.Data qualified as Data

-- Same 6 Shape types as okapi-bench (128-route HKD version)
type ListRoute    = Shape GET    ()  HTTP.Query HTTP.RequestHeaders (IO LBS.ByteString) (Data.Response S200 HTTP.ResponseHeaders (IO LBS.ByteString))
type GetRoute     = Shape GET    Int HTTP.Query HTTP.RequestHeaders (IO LBS.ByteString) (Data.Response S200 HTTP.ResponseHeaders (IO LBS.ByteString))
type PostRoute    = Shape POST   ()  HTTP.Query HTTP.RequestHeaders (IO LBS.ByteString) (Data.Response S201 HTTP.ResponseHeaders (IO LBS.ByteString))
type PutRoute     = Shape PUT    Int HTTP.Query HTTP.RequestHeaders (IO LBS.ByteString) (Data.Response S200 HTTP.ResponseHeaders (IO LBS.ByteString))
type DeleteRoute  = Shape DELETE Int HTTP.Query HTTP.RequestHeaders (IO LBS.ByteString) (Data.Response S204 HTTP.ResponseHeaders (IO LBS.ByteString))
type PostIntRoute = Shape POST   Int HTTP.Query HTTP.RequestHeaders (IO LBS.ByteString) (Data.Response S201 HTTP.ResponseHeaders (IO LBS.ByteString))

-- Canopy values (same as HKD bench)
list_      :: Canopy ListRoute
list_       = (requestGET  & path (segment_ text "r")) :-> response200

get_       :: Canopy GetRoute
get_        = (requestGET & path (segment "id" int)) :-> response200

create_    :: Canopy PostRoute
create_     = (requestPOST & path (segment_ text "r")) :-> response201

update_    :: Canopy PutRoute
update_     = (requestPUT & path (segment "id" int)) :-> response200

delete_    :: Canopy DeleteRoute
delete_     = (requestDELETE & path (segment "id" int)) :-> response204

subCreate_ :: Canopy PostIntRoute
subCreate_  = (requestPOST & path (segment "id" int)) :-> response201

-- One Handle per shape — type info erased here, not per route
listH, getH, postH, putH, deleteH, postIntH :: Handle
listH    = handle id (list_      := fn \_ -> undefined)
getH     = handle id (get_       := fn \_ -> undefined)
postH    = handle id (create_    := fn \_ -> undefined)
putH     = handle id (update_    := fn \_ -> undefined)
deleteH  = handle id (delete_    := fn \_ -> undefined)
postIntH = handle id (subCreate_ := fn \_ -> undefined)

-- 128 routes: 16 groups x 8 patterns each (list/get/create/update/delete/sub-list/sub-create/search)
-- No Generic derivation, no HKD record — just a homogeneous list
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
    ]

main :: IO ()
main = putStrLn "OK"

-- bench

-- bench2

-- bench3
