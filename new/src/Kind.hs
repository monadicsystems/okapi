{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Kind where

import Data.Kind
import GHC.TypeLits qualified as TypeLits

type VERB :: Type
data VERB where
    GET :: VERB
    POST :: VERB
    PUT :: VERB

type TREE :: Type
data TREE where
    LEAF :: VERB -> [RESPONSE] -> TREE
    NODE :: Type -> TREE -> TREE
    BRANCH :: TREE -> TREE -> TREE

type RESPONSE :: Type
data RESPONSE where
    NOCONTENT :: TypeLits.Symbol -> [TypeLits.Symbol] -> RESPONSE
    RESPONSE :: TypeLits.Symbol -> TypeLits.Nat -> [TypeLits.Symbol] -> Type -> Type -> RESPONSE

-- data Forest (f :: FOREST) (p :: [Type]) where
--     Empty :: Forest '[] p
--     Grow :: Tree t p -> Forest f p -> Forest (t : f) p
