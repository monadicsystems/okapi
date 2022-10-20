module Conduit.Model.Newtype where

import qualified Data.Aeson as Aeson
import qualified Data.Int as Int
import qualified Data.Text as Text
import qualified Rel8

newtype Email = Email {toText :: Text.Text}
  deriving newtype (Eq, Show, Rel8.DBEq, Rel8.DBType)

newtype Password = Password {toText :: Text.Text}
  deriving newtype (Eq, Show, Rel8.DBEq, Rel8.DBType)

newtype Salt = Salt {toText :: Text.Text}
  deriving newtype (Eq, Show, Rel8.DBEq, Rel8.DBType)

newtype HashedPassword = HashedPassword {toText :: Text.Text}
  deriving newtype (Eq, Show, Rel8.DBEq, Rel8.DBType)

-- | Represents a primary key or ID
newtype Key a = Key {toInt64 :: Int.Int64}
  deriving newtype (Eq, Show, Rel8.DBEq, Rel8.DBType, Aeson.FromJSON, Aeson.ToJSON)
