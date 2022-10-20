module Conduit.Model.User where

import qualified Conduit.Model.Newtype as Model
import qualified GHC.Generics as Generics
import qualified Data.Aeson as Aeson
import qualified Data.Text as Text

data User = User
  { {-key :: Model.Key User,-}
    username :: Text.Text,
    email :: Text.Text,
    imageURL :: Text.Text,
    bio :: Text.Text
  }
  deriving (Eq, Generics.Generic, Aeson.FromJSON, Aeson.ToJSON)
