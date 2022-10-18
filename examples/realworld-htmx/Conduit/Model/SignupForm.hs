module Conduit.Model.SignupForm where

import qualified Data.Aeson as Aeson
import qualified Data.Text as Text
import qualified GHC.Generics as Generics
import qualified Web.FormUrlEncoded as Web

-- TODO: Make fields appropriate newtypes
data SignupForm = SignupForm
  { email :: Text.Text,
    password :: Text.Text,
    username :: Text.Text
  }
  deriving (Generics.Generic, Show, Web.FromForm, Aeson.ToJSON)
