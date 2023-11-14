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
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use if" #-}

module Example.Calculator where

import Control.Natural qualified as Natural
import Data.Binary.Builder qualified as Builder
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Lazy.Char8 qualified as LBSChar8
import Data.Kind
import Data.List qualified as List
import Data.Text qualified as Text
import Data.Type.Equality qualified as Equality
import Data.Typeable qualified as Typeable
import Data.Vault.Lazy qualified as Vault
import GHC.Exts qualified as Exts
import GHC.Generics qualified as Generics
import GHC.Natural qualified as Natural
import Network.HTTP.Types qualified as HTTP
import Network.Wai qualified as Wai
import Network.Wai.Handler.Warp qualified as Warp
import Okapi.App {- qualified as App -}
import Okapi.Response {- qualified as Response -}
import Web.HttpApiData qualified as Web

data Operator
    = Add
    | Sub
    | Mul
    | Div
    | Sq
    | Neg
    deriving (Show)

isUnary :: Operator -> Bool
isUnary Sq = True
isUnary Neg = True
isUnary _ = False

instance Web.FromHttpApiData Operator where
    parseUrlPiece "add" = Right Add
    parseUrlPiece "sub" = Right Sub
    parseUrlPiece "minus" = Right Sub
    parseUrlPiece "mul" = Right Mul
    parseUrlPiece "div" = Right Div
    parseUrlPiece "neg" = Right Neg
    parseUrlPiece "sq" = Right Sq
    parseUrlPiece "square" = Right Sq
    parseUrlPiece _ = Left "Can't parse operator..."

unaryF =
    responder @200 @'[] @Text.Text @Int
        . responder @500 @'[] @Text.Text @Text.Text
        . method HTTP.GET id

binaryF =
    param @Int
        . responder @200 @'[] @Text.Text @Int
        . responder @500 @'[] @Text.Text @Text.Text
        . responder @403 @'[] @Text.Text @Text.Text
        . method HTTP.GET id

calc :: Node '[]
calc =
    lit "calc"
        . param @Operator
        . param @Int
        $ choice
            [ unaryF \operator x ok wrongArgs _req -> return
                $ case operator of
                    Sq -> ok noHeaders (x * x)
                    Neg -> ok noHeaders (x * (-1))
                    _ -> wrongArgs noHeaders $ Text.pack (show operator) <> " needs two arguments."
            , binaryF \operator x y ok wrongArgs divByZeroErr _req -> do
                return
                    $ case operator of
                        Add -> ok noHeaders (x + y)
                        Sub -> ok noHeaders (x - y)
                        Mul -> ok noHeaders (x * y)
                        Div ->
                            if y == 0
                                then divByZeroErr noHeaders "You can't divide by 0."
                                else ok noHeaders (div x y)
                        _ -> wrongArgs noHeaders $ Text.pack (show operator) <> " needs one argument."
            ]

calc' =
    lit "calc"
        . param @Operator
        . param @Int
        $ choice
            [ binaryF \operator x y ok wrongArgs divByZeroErr _req ->
                return $ case operator of
                    Add -> ok noHeaders (x + y)
                    Sub -> ok noHeaders (x - y)
                    Mul -> ok noHeaders (x * y)
                    Div ->
                        if y == 0
                            then divByZeroErr noHeaders "You can't divide by 0."
                            else ok noHeaders (div x y)
                    _ -> wrongArgs noHeaders $ Text.pack (show operator) <> " needs one argument."
            , unaryF \operator x ok wrongArgs _req ->
                return $ case operator of
                    Sq -> ok noHeaders (x * x)
                    Neg -> ok noHeaders (x * (-1))
                    _ -> wrongArgs noHeaders $ Text.pack (show operator) <> " needs two arguments."
            ]

main =
    Warp.run 8003 $ calc' `withDefault` \_ resp ->
        resp $ Wai.responseLBS HTTP.status404 [] "Not Found..."
