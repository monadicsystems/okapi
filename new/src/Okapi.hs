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

module Okapi where

import Control.Arrow ((>>>))
import Control.Concurrent.Chan qualified as Chan
import Control.Natural qualified as Natural
import Data.Aeson qualified as Aeson
import Data.Binary.Builder qualified as Builder
import Data.Bits (Bits (testBit))
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as Char8
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Lazy.Char8 qualified as LBSChar8
import Data.CaseInsensitive qualified as CI
import Data.Functor.Identity qualified as Identity
import Data.Kind
import Data.List qualified as List
import Data.List.NonEmpty qualified as NonEmpty
import Data.Text qualified as Text
import Data.Text.Lazy qualified as LText
import Data.Text.Lazy.Encoding qualified as Text
import Data.Tree qualified as Tree
import Data.Type.Bool (type (&&))
import Data.Type.Equality qualified as Equality
import Data.Typeable qualified as Typeable
import Data.Vault.Lazy qualified as Vault
import Data.Void qualified as Void
import GHC.Exts qualified as Exts
import GHC.Generics qualified as Generics
import GHC.Records qualified as Records
import GHC.TypeError qualified as TypeError
import GHC.TypeLits qualified as TypeLits
import GHC.TypeNats qualified as Nat
import Kind qualified
import Network.HTTP.Types qualified as HTTP
import Network.Wai qualified as Wai
import Network.Wai.EventSource qualified as Wai
import Network.Wai.Handler.Warp qualified as Warp
import Phantom qualified
import Web.HttpApiData qualified as Web

someFunc :: IO ()
someFunc = putStrLn "someFunc"

-- type FOREST :: Type
-- type FOREST = [TREE]

-- (+++) :: Forest f p -> Forest f' p -> Forest (f :+++ f') p
-- (+++) x y = case x of
--     Seed -> y
--     (Grow t f) -> Grow t (f +++ y)

type (:<>) :: [k] -> [k] -> [k]
type family (:<>) xs ys where
    '[] :<> ys = ys
    (x : xs) :<> ys = x : xs :<> ys

type (:<) :: [Type] -> Type -> [Type]
type family (:<) xs x where
    '[] :< x = x : '[]
    (x : xs) :< x' = x : (xs :< x')

type (:*) n t = Kind.NODE n t

type (:+) t t' = Kind.BRANCH t t'

class ToWaiResponseHeaders (headerKeys :: [TypeLits.Symbol]) where
    toWaiResponseHeaders :: ResponseHeaders headerKeys -> HTTP.ResponseHeaders

instance ToWaiResponseHeaders '[] where
    toWaiResponseHeaders _ = []

instance (ToWaiResponseHeaders headerKeys) => ToWaiResponseHeaders (headerKey ': headerKeys) where
    toWaiResponseHeaders (InsertHeader v tail) = [(CI.mk . Char8.pack $ TypeLits.symbolVal @headerKey Typeable.Proxy, Web.toHeader v)]

data Responses (res :: [Kind.RESPONSE]) where
    Nil :: Responses '[]
    NoContent :: forall name headers tail. (ToWaiResponseHeaders headers) => Responses tail -> Responses ('Kind.NOCONTENT name headers : tail)
    Response ::
        forall name status headers content result tail.
        (TypeLits.KnownNat status, ToContentType content result, ToWaiResponseHeaders headers) =>
        Responses tail ->
        Responses ('Kind.RESPONSE name status headers content result : tail)

noContent :: forall name headers tail. (ToWaiResponseHeaders headers) => Responses tail -> Responses ('Kind.NOCONTENT name headers : tail)
noContent = NoContent

response ::
    forall name status headers content result tail.
    (TypeLits.KnownNat status, ToContentType content result, ToWaiResponseHeaders headers) =>
    Responses tail ->
    Responses ('Kind.RESPONSE name status headers content result : tail)
response = Response

data ResponseHeaders (headerKeys :: [Exts.Symbol]) where
    NoHeaders :: ResponseHeaders '[]
    InsertHeader ::
        forall (headerKey :: Exts.Symbol) headerValue (headerKeys :: [Exts.Symbol]).
        (TypeLits.KnownSymbol headerKey, Web.ToHttpApiData headerValue) =>
        headerValue ->
        ResponseHeaders headerKeys ->
        ResponseHeaders (headerKey : headerKeys)

noHeaders :: ResponseHeaders '[]
noHeaders = NoHeaders

insertHeader ::
    forall (headerKey :: Exts.Symbol) headerValue (headerKeys :: [Exts.Symbol]).
    (TypeLits.KnownSymbol headerKey, Web.ToHttpApiData headerValue) =>
    headerValue ->
    ResponseHeaders headerKeys ->
    ResponseHeaders (headerKey : headerKeys)
insertHeader = InsertHeader

natToStatus :: Nat.Nat -> HTTP.Status
natToStatus n = toEnum $ fromEnum n

instance {-# OVERLAPS #-} Records.HasField name (Responses ('Kind.RESPONSE name status headers content result ': rs)) (ResponseHeaders headers -> result -> Wai.Response) where
    getField (Response _) headerMap result =
        let status = natToStatus $ Nat.natVal @status Typeable.Proxy
            contentType = toContentType @content @result result
            bodyType = contentTypeBody @content contentType
            name = contentTypeName @content
            headers = ("Content-Type", name) : toWaiResponseHeaders headerMap
         in case bodyType of
                ResponseBodyBytes bytes -> Wai.responseLBS status headers bytes
                ResponseBodyBuilder builder -> Wai.responseBuilder status headers builder
                ResponseBodyStream stream -> Wai.responseStream status headers stream
                ResponseBodyFile path part -> Wai.responseFile status headers path part

instance {-# OVERLAPS #-} Records.HasField name (Responses (Kind.NOCONTENT name headers ': rs)) (ResponseHeaders headers -> Wai.Response) where
    getField (NoContent _) headerMap =
        let status = natToStatus 204
            headers = toWaiResponseHeaders headerMap
         in Wai.responseLBS status headers ""

instance {-# OVERLAPPABLE #-} (Records.HasField name (Responses res) a) => Records.HasField name (Responses ('Kind.RESPONSE name' status headers content result ': res)) a where
    getField (Response r) = Records.getField @name r

instance {-# OVERLAPPABLE #-} (Records.HasField name (Responses res) a) => Records.HasField name (Responses ('Kind.NOCONTENT name' headers ': res)) a where
    getField (NoContent r) = Records.getField @name r

data ResponseBody
    = ResponseBodyStream Wai.StreamingBody
    | ResponseBodyBuilder Builder.Builder
    | ResponseBodyBytes LBS.ByteString
    | ResponseBodyFile FilePath (Maybe Wai.FilePart)

class ContentType a where
    contentTypeName :: BS.ByteString
    contentTypeBody :: a -> ResponseBody

instance ContentType Text.Text where
    contentTypeName = "text/plain"
    contentTypeBody = ResponseBodyBytes . Text.encodeUtf8 . LText.fromStrict

instance ContentType Aeson.Value where
    contentTypeName = "application/json"
    contentTypeBody = ResponseBodyBytes . Aeson.encode

class (ContentType a) => ToContentType a b where
    toContentType :: b -> a

instance ToContentType Text.Text Text.Text where
    toContentType = id

instance ToContentType Text.Text Int where
    toContentType = Text.pack . show

instance (Aeson.ToJSON a) => ToContentType Aeson.Value a where
    toContentType = Aeson.toJSON

data Env res = Env
    { request :: Wai.Request
    , responses :: Responses res
    }

type Handler :: [Kind.RESPONSE] -> [Type] -> (Type -> Type) -> Type
type family Handler res p env where
    Handler res '[] env = Env res -> env Wai.Response
    Handler res (Phantom.Lit s : rem) env = Handler res rem env
    Handler res (Phantom.Param a : rem) env = a -> Handler res rem env
    Handler res (Phantom.Splat a : rem) env = NonEmpty.NonEmpty a -> Handler res rem env
    -- Handler (Phantom.Response status headers content result : rem) env = Phantom.Response status headers content result -> Handler rem env
    Handler res x _ = TypeError.TypeError (TypeError.Text "Can't create Handler for type: " TypeError.:<>: TypeError.ShowType x)

type Root (t :: Kind.TREE) = Tree t '[]

data Tree (t :: Kind.TREE) (p :: [Type]) where
    Method ::
        forall (v :: Kind.VERB) (env :: Type -> Type) (res :: [Kind.RESPONSE]) (p :: [Type]).
        (Responses '[] -> Responses res) ->
        (env Natural.~> IO) ->
        (Handler res p env) ->
        Tree (Kind.LEAF v res) p
    Lit ::
        forall (s :: Exts.Symbol) (c :: Kind.TREE) (p :: [Type]).
        (TypeLits.KnownSymbol s) =>
        Tree c (p :< Phantom.Lit s) ->
        Tree (Phantom.Lit s :* c) p
    Param ::
        forall (a :: Type) (c :: Kind.TREE) (p :: [Type]).
        (Web.FromHttpApiData a) =>
        Tree c (p :< Phantom.Param a) ->
        Tree (Phantom.Param a :* c) p
    Splat ::
        forall a (c :: Kind.TREE) (p :: [Type]).
        (Web.FromHttpApiData a) =>
        Tree c (p :< Phantom.Splat a) ->
        Tree (Phantom.Splat a :* c) p
    -- Response ::
    --     forall (status :: Nat.Nat) (headers :: [Exts.Symbol]) (content :: Type) (result :: Type) (t :: Kind.TREE) (c :: Kind.TREE) (p :: [Type]).
    --     ( Nat.KnownNat status
    --     , t ~ Phantom.Response status headers content result :* c
    --     ) =>
    --     Tree c (p :< Phantom.Response status headers content result) ->
    --     Tree t p
    Branch ::
        forall (t :: Kind.TREE) (t' :: Kind.TREE) (p :: [Type]).
        (Valid (t :+ t') ~ True) =>
        Tree t p ->
        Tree t' p ->
        Tree (t :+ t') p

method ::
    forall (v :: Kind.VERB) (env :: Type -> Type) (p :: [Type]) (res :: [Kind.RESPONSE]).
    (Responses '[] -> Responses res) ->
    (env Natural.~> IO) ->
    Handler res p env ->
    Tree (Kind.LEAF v res) p
method = Method

lit ::
    forall (s :: Exts.Symbol) (c :: Kind.TREE) (p :: [Type]).
    (TypeLits.KnownSymbol s) =>
    Tree c (p :< Phantom.Lit s) ->
    Tree (Phantom.Lit s :* c) p
lit = Lit

param ::
    forall (a :: Type) (c :: Kind.TREE) (p :: [Type]).
    (Web.FromHttpApiData a) =>
    Tree c (p :< Phantom.Param a) ->
    Tree (Phantom.Param a :* c) p
param = Param

splat ::
    forall a (c :: Kind.TREE) (p :: [Type]).
    (Web.FromHttpApiData a) =>
    Tree c (p :< Phantom.Splat a) ->
    Tree (Phantom.Splat a :* c) p
splat = Splat

-- response ::
--     forall (status :: Nat.Nat) (headers :: [Exts.Symbol]) (content :: Type) (result :: Type) (t :: Kind.TREE) (c :: Kind.TREE) (p :: [Type]).
--     ( Nat.KnownNat status
--     , t ~ Phantom.Response status headers content result :* c
--     ) =>
--     Tree c (p :< Phantom.Response status headers content result) ->
--     Tree t p
-- response = Response

infixr 6 |||

(|||) ::
    forall (t :: Kind.TREE) (t' :: Kind.TREE) (p :: [Type]).
    (Valid (t :+ t') ~ True) =>
    Tree t p ->
    Tree t' p ->
    Tree (t :+ t') p
(|||) = Branch

type Valid :: Kind.TREE -> Bool
type family Valid api where
    Valid t = ValidHelper '[] '[] t

type ValidHelper :: [Type] -> [Kind.VERB] -> Kind.TREE -> Bool
type family ValidHelper nodes leaves t where
    ValidHelper seenNodes seenLeaves (Kind.LEAF v _) = NotElem v seenLeaves
    ValidHelper seenNodes seenLeaves (Kind.NODE n st) =
        NotElem n seenNodes && Valid st
    ValidHelper seenNodes seenLeaves (Kind.BRANCH (Kind.LEAF v _) t') =
        NotElem v seenLeaves && ValidHelper seenNodes (v : seenLeaves) t'
    ValidHelper seenNodes seenLeaves (Kind.BRANCH (Kind.NODE n st) t') =
        NotElem n seenNodes && Valid st && ValidHelper (n : seenNodes) seenLeaves t'
    ValidHelper seenNodes seenLeaves (Kind.BRANCH t t') =
        ValidHelper seenNodes seenLeaves t && ValidHelper seenNodes seenLeaves t'

type NotElem :: k -> [k] -> Bool
type family NotElem k ks where
    NotElem k '[] = True
    NotElem k (k : ks) = False
    NotElem k (k' : ks) = True && NotElem k ks

type Endpoints :: Kind.TREE -> [([Type], Kind.VERB)]
type family Endpoints api where
    Endpoints (Kind.BRANCH a b) = Endpoints a :<> Endpoints b
    Endpoints (Kind.NODE n a) = MapSub n (Endpoints a)
    Endpoints (Kind.LEAF v _) = '[ '( '[], v)]

type MapSub :: Type -> [([Type], Kind.VERB)] -> [([Type], Kind.VERB)]
type family MapSub e xs where
    MapSub e '[] = '[]
    MapSub e ('(ts, v) ': xs) = '(e ': ts, v) ': MapSub e xs

api :: Root _
api = home homeHandler ||| person personHandler

home =
    lit @"hello"
        . lit @"world"
        . param @Text.Text
        . method @Kind.GET @IO (response @"ok" @200 @'[] @Text.Text @Text.Text . response @"error" @500 @'[] @Text.Text @Text.Text) id

homeHandler (name :: Text.Text) env =
    if name == "Bob"
        then return $ env.responses.ok noHeaders "Hello"
        else return $ env.responses.error noHeaders "Bye"

person = lit @"person" . method @Kind.PUT @IO (response @"ok" @200 @'[] @Text.Text @Text.Text) id

personHandler env = return $ env.responses.ok noHeaders "Ping"

{-
get :: env Natural.~> IO -> Handler p2 env -> Tree ('Kind.LEAF 'Kind.GET res) p2
get trans f = method @Kind.GET trans f

getIO = get id

getP = get @Identity.Identity (pure . Identity.runIdentity)
-}
{-
any :: forall env p. env Natural.~> IO -> Handler p env -> Tree ('Kind.LEAF 'Kind.GET :+ ('Kind.LEAF 'Kind.POST :+ 'Kind.LEAF 'Kind.PUT)) p
any trans f = method @Kind.GET @env @p trans f ||| method @Kind.POST trans f ||| method @Kind.PUT trans f
-}
