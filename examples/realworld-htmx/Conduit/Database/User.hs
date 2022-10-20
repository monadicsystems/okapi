module Conduit.Database.User where

import qualified Conduit.Model.Newtype as Model
import qualified Conduit.Model.SignupForm as Model.SignupForm
import qualified Conduit.Model.User as Model.User
import qualified Data.Text as Text
import qualified GHC.Generics as Generics
import qualified Rel8
import Rel8 ((==.))

data UserRow f = UserRow
  { key :: Rel8.Column f (Model.Key Model.User.User),
    username :: Rel8.Column f Text.Text,
    email :: Rel8.Column f Text.Text,
    bio :: Rel8.Column f Text.Text,
    imageURL :: Rel8.Column f Text.Text,
    salt :: Rel8.Column f Model.Salt,
    hashedPassword :: Rel8.Column f Model.HashedPassword
  }
  deriving stock (Generics.Generic)
  deriving anyclass (Rel8.Rel8able)

deriving stock instance f ~ Rel8.Result => Show (UserRow f)

userTableSchema :: Rel8.TableSchema (UserRow Rel8.Name)
userTableSchema =
  Rel8.TableSchema
    { name = "users",
      schema = Nothing,
      columns =
        UserRow
          { key = "pk_user",
            username = "username",
            email = "email",
            bio = "bio",
            imageURL = "image_url",
            salt = "salt",
            hashedPassword = "hashed_password"
          }
    }

insertNewUser :: Model.SignupForm.SignupForm -> (Model.Salt, Model.HashedPassword) -> Rel8.Insert [Model.Key Model.User.User]
insertNewUser signupForm (salt, hashedPassword) =
  Rel8.Insert
    { into = userTableSchema,
      rows =
        Rel8.values
          [ UserRow
              { key = Rel8.unsafeCastExpr $ Rel8.nextval "users_user_id_seq", -- TODO: Check if this is correct
                username = Rel8.lit $ Model.SignupForm.username signupForm,
                email = Rel8.lit $ Model.SignupForm.email signupForm,
                bio = Rel8.lit "",
                imageURL = Rel8.lit "https://api.realworld.io/images/smiley-cyrus.jpeg",
                salt = Rel8.lit salt,
                hashedPassword = Rel8.lit hashedPassword
              }
          ],
      onConflict = Rel8.DoNothing,
      returning = Rel8.Projection key
    }

userRowToUser :: UserRow Rel8.Result -> Model.User.User
userRowToUser row =
  Model.User.User
    { username = username row,
      email = email row,
      imageURL = imageURL row,
      bio = bio row
    }

queryUserByKey :: Model.Key Model.User.User -> Rel8.Query (UserRow Rel8.Expr)
queryUserByKey userKey = do
  a <- Rel8.each userTableSchema
  Rel8.where_ $ key a ==. Rel8.lit userKey
  return a
