module Okapi.Artifact.Function (
    Function (..),
    fn,
) where

import Data.Kind (Type)
import Network.Wai qualified as Wai
import Okapi.HTTP (Shape)
import Okapi.Data.Request qualified as Data

data Function (n :: Type -> Type) shape where
    Function ::
        ((Data.Request method path query headers body, Wai.Request) -> n result) ->
        Function n (Shape method path query headers body result)

fn ::
    ((Data.Request method path query headers body, Wai.Request) -> n result) ->
    Function n (Shape method path query headers body result)
fn = Function
