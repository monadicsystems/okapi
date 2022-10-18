module Conduit.Validate where

import qualified Conduit.Model.SigninForm as Model
import qualified Conduit.Model.SignupForm as Model
import Control.Monad ((>=>))
import qualified Control.Monad.Except as Except
import qualified Control.Monad.Identity as Identity
import qualified Control.Monad.Trans.Except as ExceptT
import qualified Data.Aeson as Aeson
import qualified Data.Maybe as Maybe
import qualified Data.Text as Text
import qualified Web.Forma as Forma

type SignupFormFields = '["email", "password", "username"]

signupForm ::
  Model.SignupForm ->
  Forma.FormResult SignupFormFields (Text.Text -> Text.Text) Model.SignupForm
signupForm = Forma.runFormPure validator . Aeson.toJSON
  where
    validator :: Monad m => Forma.FormParser SignupFormFields (Text.Text -> Text.Text) m Model.SignupForm
    validator =
      Model.SignupForm
        <$> Forma.field #email (notEmpty >=> validEmail)
        <*> Forma.field #password (notEmpty >=> tooShort)
        <*> Forma.field #username notEmpty

type SigninFormFields = '["email", "password"]

signinForm ::
  Model.SigninForm ->
  Forma.FormResult SigninFormFields (Text.Text -> Text.Text) Model.SigninForm
signinForm = Forma.runFormPure validator . Aeson.toJSON
  where
    validator :: Monad m => Forma.FormParser SigninFormFields (Text.Text -> Text.Text) m Model.SigninForm
    validator =
      Model.SigninForm
        <$> Forma.field #email (notEmpty >=> validEmail)
        <*> Forma.field #password (notEmpty >=> tooShort)

notEmpty :: Monad m => Text.Text -> Except.ExceptT (Text.Text -> Text.Text) m Text.Text
notEmpty txt =
  if Text.null txt
    then ExceptT.throwE (<> " can't be blank")
    else pure txt

tooShort :: Monad m => Text.Text -> Except.ExceptT (Text.Text -> Text.Text) m Text.Text
tooShort txt =
  if Text.length txt < 6
    then ExceptT.throwE (<> " must be 6 characters or more")
    else pure txt

validEmail :: Monad m => Text.Text -> Except.ExceptT (Text.Text -> Text.Text) m Text.Text
validEmail txt =
  if Maybe.isNothing $ Text.find (== '@') txt
    then ExceptT.throwE (<> " must contain @ symbol to be valid")
    else pure txt
