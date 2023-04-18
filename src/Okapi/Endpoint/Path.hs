{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Okapi.Endpoint.Path where

import qualified Control.Monad.Par as Par
import qualified Data.Functor.Classes as Functor
import qualified Data.OpenApi as OAPI
import qualified Data.Text as Text
import qualified Data.Typeable as Typeable
import qualified Debug.Trace as Debug
import qualified GHC.Generics as Generics
import Generics.Kind
import Generics.Kind.TH
import qualified Web.HttpApiData as Web

data Error
  = ParseFail
  | NoMatch
  | EmptyPath
  | TooManyOperations
  | NotEnoughOperations
  deriving (Eq, Show, Generics.Generic, Par.NFData)

data Path a where
  FMap :: (a -> b) -> Path a -> Path b
  Pure :: a -> Path a
  Apply :: Path (a -> b) -> Path a -> Path b
  Static :: Text.Text -> Path ()
  Param :: (Typeable.Typeable a, Web.FromHttpApiData a, OAPI.ToSchema a) => Text.Text -> Path a

$(deriveGenericK ''Path)

{-
instance GenericK (Path (a_a11LZ :: ghc-prim:GHC.Types.Type) :: ghc-prim:GHC.Types.Type) where
  type RepK (Path (a_a11LZ :: ghc-prim:GHC.Types.Type) :: ghc-prim:GHC.Types.Type) =
    Generics.D1
      ('Generics.MetaData "Path" "Okapi.Endpoint.Path" "okapi-0.2.0.0-inplace" 'False)
      ((:+:)
         ((:+:)
            (Generics.C1 ('Generics.MetaCons "FMap" 'Generics.PrefixI 'False)
                          (Exists ghc-prim:GHC.Types.Type
                                  ((:*:)
                                     (Generics.S1 ('Generics.MetaSel 'Nothing 'Generics.NoSourceUnpackedness 'Generics.NoSourceStrictness 'Generics.DecidedLazy)
                                                   (Field ('(:@:) ('(:@:) ('Kon (->)) Var0) ('Kon a_a11LZ))))
                                     (Generics.S1 ('Generics.MetaSel 'Nothing 'Generics.NoSourceUnpackedness 'Generics.NoSourceStrictness 'Generics.DecidedLazy)
                                                   (Field ('(:@:) ('Kon Path) Var0))))))
            (Generics.C1 ('Generics.MetaCons "Pure" 'Generics.PrefixI 'False)
                          (Generics.S1 ('Generics.MetaSel 'Nothing 'Generics.NoSourceUnpackedness 'Generics.NoSourceStrictness 'Generics.DecidedLazy)
                                        (Field ('Kon a_a11LZ))))))
      ((:+:)
         (Generics.C1 ('Generics.MetaCons "Apply" 'Generics.PrefixI 'False)
                       (Exists ghc-prim:GHC.Types.Type
                               ((:*:)
                                  (Generics.S1 ('Generics.MetaSel 'Nothing 'Generics.NoSourceUnpackedness 'Generics.NoSourceStrictness 'Generics.DecidedLazy)
                                                (Field ('(:@:) ('Kon Path)
                                                              ('(:@:) ('(:@:) ('Kon (->)) Var0) ('Kon a_a11LZ)))))
                                  (Generics.S1 ('Generics.MetaSel 'Nothing 'Generics.NoSourceUnpackedness 'Generics.NoSourceStrictness 'Generics.DecidedLazy)
                                                (Field ('(:@:) ('Kon Path) Var0))))))
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

instance Functor Path where
  fmap = FMap

instance Applicative Path where
  pure = Pure
  (<*>) = Apply

-- TODO: Try using Rebindable Syntax and QualifiedDo to get rid of
-- the need for Functor and Applicative instances.

static :: Text.Text -> Path ()
static = Static

param :: (Typeable.Typeable a, Web.FromHttpApiData a, OAPI.ToSchema a) => Text.Text -> Path a
param = Param

eval ::
  Path a ->
  [Text.Text] ->
  (Either Error a, [Text.Text])
eval path state = case compare (countOps path) (length state) of
  LT -> (Left NotEnoughOperations, state)
  GT -> (Left TooManyOperations, state)
  EQ -> loop path state
  where
    loop ::
      Path a ->
      [Text.Text] ->
      (Either Error a, [Text.Text])
    loop path state = case path of
      FMap f opX ->
        case loop opX state of
          (Left e, state') -> (Left e, state')
          (Right x, state') -> (Right $ f x, state')
      Pure x -> (Right x, state)
      Apply opF opX -> case loop opF state of
        (Right f, state') -> case loop opX state' of
          (Right x, state'') -> (Right $ f x, state'')
          (Left e, state'') -> (Left e, state'')
        (Left e, state') -> (Left e, state')
      Static goal -> case state of
        [] -> (Left EmptyPath, state)
        (h : t) ->
          if h == goal
            then (Right (), t)
            else (Left NoMatch, state)
      Param _ -> case state of
        [] -> (Left EmptyPath, state)
        (h : t) -> case Web.parseUrlPieceMaybe h of
          Nothing -> (Left ParseFail, state)
          Just v -> (Right v, t)

countOps :: Path a -> Int
countOps path = case path of
  FMap _ opX -> countOps opX
  Pure _ -> 0
  Apply opF opX -> countOps opF + countOps opX
  Static _ -> 1
  Param _ -> 1

renderPath :: Path a -> Text.Text
renderPath path = case path of
  FMap f p -> renderPath p
  Pure _ -> mempty
  Apply pf px -> renderPath pf <> renderPath px
  Static t -> "/" <> t
  Param @p name -> "/{" <> name <> ":" <> Text.pack (show $ Typeable.typeOf @p undefined) <> "}"
