{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Okapi.Script.Path where

import Control.Monad.Par qualified as Par
import Data.Functor.Classes qualified as Functor
import Data.OpenApi qualified as OAPI
import Data.Text qualified as Text
import Data.Typeable qualified as Typeable
import Debug.Trace qualified as Debug
import GHC.Generics qualified as Generics
import Generics.Kind
import Generics.Kind.TH
import Okapi.Script
import Web.HttpApiData qualified as Web

data Error
  = ParseFail
  | NoMatch
  | EmptyPath
  | TooManyOperations
  | NotEnoughOperations
  deriving (Eq, Show, Generics.Generic, Par.NFData)

data Script a where
  FMap :: (a -> b) -> Script a -> Script b
  Pure :: a -> Script a
  Apply :: Script (a -> b) -> Script a -> Script b
  Static :: Text.Text -> Script ()
  Param :: (Typeable.Typeable a, Web.FromHttpApiData a, OAPI.ToSchema a) => Text.Text -> Script a

$(deriveGenericK ''Script)

{-
instance GenericK (Script (a_a11LZ :: ghc-prim:GHC.Types.Type) :: ghc-prim:GHC.Types.Type) where
  type RepK (Script (a_a11LZ :: ghc-prim:GHC.Types.Type) :: ghc-prim:GHC.Types.Type) =
    Generics.D1
      ('Generics.MetaData "Script" "Okapi.Endpoint.Script" "okapi-0.2.0.0-inplace" 'False)
      ((:+:)
         ((:+:)
            (Generics.C1 ('Generics.MetaCons "FMap" 'Generics.PrefixI 'False)
                          (Exists ghc-prim:GHC.Types.Type
                                  ((:*:)
                                     (Generics.S1 ('Generics.MetaSel 'Nothing 'Generics.NoSourceUnpackedness 'Generics.NoSourceStrictness 'Generics.DecidedLazy)
                                                   (Field ('(:@:) ('(:@:) ('Kon (->)) Var0) ('Kon a_a11LZ))))
                                     (Generics.S1 ('Generics.MetaSel 'Nothing 'Generics.NoSourceUnpackedness 'Generics.NoSourceStrictness 'Generics.DecidedLazy)
                                                   (Field ('(:@:) ('Kon Script) Var0))))))
            (Generics.C1 ('Generics.MetaCons "Pure" 'Generics.PrefixI 'False)
                          (Generics.S1 ('Generics.MetaSel 'Nothing 'Generics.NoSourceUnpackedness 'Generics.NoSourceStrictness 'Generics.DecidedLazy)
                                        (Field ('Kon a_a11LZ))))))
      ((:+:)
         (Generics.C1 ('Generics.MetaCons "Apply" 'Generics.PrefixI 'False)
                       (Exists ghc-prim:GHC.Types.Type
                               ((:*:)
                                  (Generics.S1 ('Generics.MetaSel 'Nothing 'Generics.NoSourceUnpackedness 'Generics.NoSourceStrictness 'Generics.DecidedLazy)
                                                (Field ('(:@:) ('Kon Script)
                                                              ('(:@:) ('(:@:) ('Kon (->)) Var0) ('Kon a_a11LZ)))))
                                  (Generics.S1 ('Generics.MetaSel 'Nothing 'Generics.NoSourceUnpackedness 'Generics.NoSourceStrictness 'Generics.DecidedLazy)
                                                (Field ('(:@:) ('Kon Script) Var0))))))
         ((:+:)
            (Generics.C1 ('Generics.MetaCons "Static" 'Generics.PrefixI 'False)
                          ((:=>:) ('Kon ((~~) a_a11LZ ()))
                                   (Generics.S1 ('Generics.MetaSel 'Nothing 'Generics.NoSourceUnpackedness 'Generics.NoSourceStrictness 'Generics.DecidedLazy)
                                                 (Field ('Kon Text.Text)))))
            (Generics.C1 ('Generics.MetaCons "Param" 'Generics.PrefixI 'False)
                          ((:=>:) ('(:&:) ('Kon (Typeable.Typeable a_a11LZ))
                                          ('Kon (Web.FromHttpApiData a_a11LZ))) U1)))))
  fromK x_a11V7 =
    M1
      (case x_a11V7 of
         FMap f1_a11V8 f2_a11V9 ->
           L1 (L1 (M1 (Exists (M1 (Field f1_a11V8) :*: M1 (Field f2_a11V9)))))
         Pure f1_a11Va -> L1 (R1 (M1 (M1 (Field f1_a11Va...
-}

instance Functor Script where
  fmap = FMap

instance Applicative Script where
  pure = Pure
  (<*>) = Apply

-- TODO: Try using Rebindable Syntax and QualifiedDo to get rid of
-- the need for Functor and Applicative instances.

static :: Text.Text -> Script ()
static = Static

param :: (Typeable.Typeable a, Web.FromHttpApiData a, OAPI.ToSchema a) => Text.Text -> Script a
param = Param

eval ::
  Script a ->
  [Text.Text] ->
  (Result Error a, [Text.Text])
eval path state = case compare (countOps path) (length state) of
  LT -> (Fail NotEnoughOperations, state)
  GT -> (Fail TooManyOperations, state)
  EQ -> loop path state
  where
    loop ::
      Script a ->
      [Text.Text] ->
      (Result Error a, [Text.Text])
    loop path state = case path of
      FMap f opX ->
        case loop opX state of
          (Fail e, state') -> (Fail e, state')
          (Ok x, state') -> (Ok $ f x, state')
      Pure x -> (Ok x, state)
      Apply opF opX -> case loop opF state of
        (Ok f, state') -> case loop opX state' of
          (Ok x, state'') -> (Ok $ f x, state'')
          (Fail e, state'') -> (Fail e, state'')
        (Fail e, state') -> (Fail e, state')
      Static goal -> case state of
        [] -> (Fail EmptyPath, state)
        (h : t) ->
          if h == goal
            then (Ok (), t)
            else (Fail NoMatch, state)
      Param _ -> case state of
        [] -> (Fail EmptyPath, state)
        (h : t) -> case Web.parseUrlPieceMaybe h of
          Nothing -> (Fail ParseFail, state)
          Just v -> (Ok v, t)

countOps :: Script a -> Int
countOps path = case path of
  FMap _ opX -> countOps opX
  Pure _ -> 0
  Apply opF opX -> countOps opF + countOps opX
  Static _ -> 1
  Param _ -> 1

renderPath :: Script a -> Text.Text
renderPath path = case path of
  FMap f p -> renderPath p
  Pure _ -> mempty
  Apply pf px -> renderPath pf <> renderPath px
  Static t -> "/" <> t
  Param @p name -> "/{" <> name <> ":" <> Text.pack (show $ Typeable.typeOf @p undefined) <> "}"
