module Okapi.Function (
    Function (..),
    fn,
) where

import Data.Kind (Type)
import Network.Wai qualified as Wai
import Okapi.HTTP (Signature)
import Okapi.Request.Data qualified as Data

data Function (n :: Type -> Type) shape where
    Function ::
        ((Data.Request method path query headers body, Wai.Request) -> n result) ->
        Function n (Signature method path query headers body result)

fn ::
    ((Data.Request method path query headers body, Wai.Request) -> n result) ->
    Function n (Signature method path query headers body result)
fn = Function
