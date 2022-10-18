module Conduit.Model.SigninForm where

import qualified Data.Text as Text
import qualified GHC.Generics as Generics
import qualified Web.FormUrlEncoded as Web

data SigninForm = SigninForm
  { email :: Text.Text,
    password :: Text.Text
  }
  deriving (Generics.Generic, Show, Web.FromForm)
