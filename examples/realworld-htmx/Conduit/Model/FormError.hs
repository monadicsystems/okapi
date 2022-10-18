module Conduit.Model.FormError where

import qualified Data.Text as Text
import Lucid

newtype FormError = FormError Text.Text
  deriving newtype (Eq, Show, Semigroup, Monoid, ToHtml)
