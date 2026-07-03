
module Okapi.Mode.Forest (
    Shape,
    Forest (..),
) where

import Data.Kind (Type)
import Okapi.Record.Tree qualified as Tree
import Okapi.Record.Data qualified as Data
import Okapi.HTTP.Responses (Cases, Responses)

data Shape
    (method  :: Type)
    (path    :: Type)
    (query   :: Type)
    (headers :: Type)
    (body    :: Type)
    (result  :: Type)

data Forest shape where
    (:->) ::
        Tree.Request method path query headers body ->
        Tree.Response status resHeaders resBody ->
        Forest (Shape method path query headers body (Data.Response status resHeaders resBody))
    (:-<) ::
        Cases responses =>
        Tree.Request method path query headers body ->
        Responses Tree.Response responses ->
        Forest (Shape method path query headers body (responses Data.Response))
