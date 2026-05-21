{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveLift #-}

module TH where
import Data.Text
import Language.Haskell.TH.Syntax

data Response = Response

data App where
    GET :: Text -> Maybe a -> (a -> IO Response) -> App
    Route :: Text -> App -> App
    (:+) :: App -> App -> App

instance Lift App where

infixl 2 :+

get = GET
route = Route
none = Nothing

handler = \ctx -> do
    return Response

someRoutes =
    route "/boo"
        $ get "/foo" none handler
        :+ get "/baz" none handler
ex1 =
    get "/home" none handler
        :+ someRoutes
        :+ get
            "/bar"
            none
            ( \ctx -> do
                return Response
            )
        :+ get "/far" none handler
