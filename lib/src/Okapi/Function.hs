module Okapi.Function (
    Function (..),
    fn,
) where

import Data.Kind (Type)
import Network.Wai qualified as Wai
import Okapi.Contract (Signature)
import Okapi.HTTP.Request qualified as Req

data Function (n :: Type -> Type) shape where
    Function ::
        ((Req.Data method path query headers body, Wai.Request) -> n result) ->
        Function n (Signature method path query headers body result)

fn ::
    ((Req.Data method path query headers body, Wai.Request) -> n result) ->
    Function n (Signature method path query headers body result)
fn = Function
