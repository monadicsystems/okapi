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
import Data.Either qualified as Either
import Data.Functor.Identity qualified as Identity
import Data.Kind
import Data.List qualified as List
import Data.List.Extra qualified as Extra
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
import Network.HTTP.Types qualified as HTTP
import Network.Wai qualified as Wai
import Network.Wai.EventSource qualified as Wai
import Network.Wai.Handler.Warp qualified as Warp
import Web.HttpApiData qualified as Web

-----------
-- KINDS --
-----------

type VERB :: Type
data VERB where
    GET :: VERB
    POST :: VERB
    PUT :: VERB

class ToStdMethod (v :: VERB) where
    toStdMethod :: HTTP.StdMethod

instance ToStdMethod GET where
    toStdMethod = HTTP.GET

instance ToStdMethod POST where
    toStdMethod = HTTP.POST

instance ToStdMethod PUT where
    toStdMethod = HTTP.PUT

type TREE :: Type
data TREE where
    LEAF :: TypeLits.Symbol -> [Type] -> (Type -> Type) -> VERB -> [RESPONSE] -> TREE
    NODE :: Type -> TREE -> TREE
    BRANCH :: TREE -> TREE -> TREE

type (:*) n t = NODE n t

type (:+) t t' = BRANCH t t'

--------------
-- PHANTOMS --
--------------

data Lit_ (s :: TypeLits.Symbol) where
    Lit_ :: (TypeLits.KnownSymbol s) => Lit_ s

data Param_ (a :: Type) where
    Param_ :: (Web.FromHttpApiData a, Web.ToHttpApiData a) => a -> Param_ a

data Splat_ (a :: Type) where
    Splat_ :: (Web.FromHttpApiData a) => NonEmpty.NonEmpty a -> Splat_ a

data Method_ (name :: TypeLits.Symbol) (verb :: VERB) (env :: Type -> Type) (res :: [RESPONSE]) (path :: [Type]) where
    Method_ ::
        forall name verb env res path.
        (ToStdMethod verb, BuildHandler res path env, URLBuilder path) =>
        (env Natural.~> IO) ->
        (Responses '[] -> Responses res) ->
        (Handler res path env) ->
        Method_ name verb env res path

class URLBuilder (path :: [Type]) where
    type URLBuilderType path :: Type
    buildURL :: [Text.Text] -> URLBuilderType path

instance URLBuilder '[] where
    type URLBuilderType '[] = [Text.Text]
    buildURL = id

instance (TypeLits.KnownSymbol s, URLBuilder rem) => URLBuilder (Lit_ s : rem) where
    type URLBuilderType (Lit_ s : rem) = URLBuilderType rem
    buildURL list = buildURL @rem (Extra.snoc list (Text.pack $ TypeLits.symbolVal @s Typeable.Proxy))

instance (URLBuilder rem, Web.ToHttpApiData a) => URLBuilder (Param_ a : rem) where
    type URLBuilderType (Param_ a : rem) = a -> URLBuilderType rem
    buildURL list x = buildURL @rem (Extra.snoc list (Web.toUrlPiece x))

instance (URLBuilder rem, Web.ToHttpApiData a) => URLBuilder (Splat_ a : rem) where
    type URLBuilderType (Splat_ a : rem) = NonEmpty.NonEmpty a -> URLBuilderType rem
    buildURL list nel = buildURL @rem (list <> (reverse $ NonEmpty.toList (fmap Web.toUrlPiece nel)))

instance (f ~ Handler res path env) => Records.HasField "handler" (Method_ name verb env res path) f where
    getField (Method_ _ _ handler) = handler

instance (URLBuilder path, f ~ URLBuilderType path) => Records.HasField "url" (Method_ name verb env res path) f where
    getField (Method_ _ _ _) = buildURL @path []

----------
-- Tree --
----------

data Tree (t :: TREE) (p :: [Type]) where
    Method ::
        forall (name :: TypeLits.Symbol) (v :: VERB) (env :: Type -> Type) (res :: [RESPONSE]) (p :: [Type]).
        (ToStdMethod v, BuildHandler res p env, URLBuilder p) =>
        (env Natural.~> IO) ->
        (Responses '[] -> Responses res) ->
        (Handler res p env) ->
        Tree (LEAF name p env v res) p
    Lit ::
        forall (s :: Exts.Symbol) (c :: TREE) (p :: [Type]).
        (TypeLits.KnownSymbol s) =>
        Tree c (p :< Lit_ s) ->
        Tree (Lit_ s :* c) p
    Param ::
        forall (a :: Type) (c :: TREE) (p :: [Type]).
        (Web.FromHttpApiData a, Web.ToHttpApiData a) =>
        Tree c (p :< Param_ a) ->
        Tree (Param_ a :* c) p
    Splat ::
        forall a (c :: TREE) (p :: [Type]).
        (Web.FromHttpApiData a, Web.ToHttpApiData a) =>
        Tree c (p :< Splat_ a) ->
        Tree (Splat_ a :* c) p
    Branch ::
        forall (t :: TREE) (t' :: TREE) (p :: [Type]).
        (Valid (t :+ t') ~ True) =>
        Tree t p ->
        Tree t' p ->
        Tree (t :+ t') p

type Root (t :: TREE) = Tree t '[]

method ::
    forall (name :: TypeLits.Symbol) (v :: VERB) (env :: Type -> Type) (res :: [RESPONSE]) (p :: [Type]).
    (ToStdMethod v, BuildHandler res p env, URLBuilder p) =>
    (env Natural.~> IO) ->
    (Responses '[] -> Responses res) ->
    (Handler res p env) ->
    Tree (LEAF name p env v res) p
method = Method

lit ::
    forall (s :: Exts.Symbol) (c :: TREE) (p :: [Type]).
    (TypeLits.KnownSymbol s) =>
    Tree c (p :< Lit_ s) ->
    Tree (Lit_ s :* c) p
lit = Lit

param ::
    forall (a :: Type) (c :: TREE) (p :: [Type]).
    (Web.FromHttpApiData a, Web.ToHttpApiData a) =>
    Tree c (p :< Param_ a) ->
    Tree (Param_ a :* c) p
param = Param

splat ::
    forall a (c :: TREE) (p :: [Type]).
    (Web.FromHttpApiData a, Web.ToHttpApiData a) =>
    Tree c (p :< Splat_ a) ->
    Tree (Splat_ a :* c) p
splat = Splat

infixr 6 |||

(|||) ::
    forall (t :: TREE) (t' :: TREE) (p :: [Type]).
    (Valid (t :+ t') ~ True) =>
    Tree t p ->
    Tree t' p ->
    Tree (t :+ t') p
(|||) = Branch

-- instance (Records.HasField name (Tree c (p :< Lit_ s)) (Method_ name verb env res path)) => Records.HasField name (Tree (NODE (Lit_ s) c) p) (Method_ name verb env res path) where
--     getField (Lit child) = Records.getField @name child

instance (TreeToMethods t, Records.HasField name (Methods (TTMType t)) (Method_ name verb env res path)) => Records.HasField name (Tree t p) (Method_ name verb env res path) where
    getField tree = Records.getField @name $ treeToMethods tree

---------------
-- RESPONSES --
---------------

type RESPONSE :: Type
data RESPONSE where
    NOCONTENT :: TypeLits.Symbol -> [TypeLits.Symbol] -> RESPONSE
    RESPONSE :: TypeLits.Symbol -> TypeLits.Nat -> [TypeLits.Symbol] -> Type -> Type -> RESPONSE

data Responses (res :: [RESPONSE]) where
    Nil :: Responses '[]
    NoContent :: forall name headers tail. (ToWaiResponseHeaders headers) => Responses tail -> Responses ('NOCONTENT name headers : tail)
    Response ::
        forall name status headers content result tail.
        (TypeLits.KnownNat status, ToContentType content result, ToWaiResponseHeaders headers) =>
        Responses tail ->
        Responses ('RESPONSE name status headers content result : tail)

noContent :: forall name headers tail. (ToWaiResponseHeaders headers) => Responses tail -> Responses ('NOCONTENT name headers : tail)
noContent = NoContent

response ::
    forall name status headers content result tail.
    (TypeLits.KnownNat status, ToContentType content result, ToWaiResponseHeaders headers) =>
    Responses tail ->
    Responses ('RESPONSE name status headers content result : tail)
response = Response

instance {-# OVERLAPS #-} Records.HasField name (Responses ('RESPONSE name status headers content result ': rs)) (ResponseHeaders headers -> result -> Wai.Response) where
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

instance {-# OVERLAPS #-} Records.HasField name (Responses (NOCONTENT name headers ': rs)) (ResponseHeaders headers -> Wai.Response) where
    getField (NoContent _) headerMap =
        let status = natToStatus 204
            headers = toWaiResponseHeaders headerMap
         in Wai.responseLBS status headers ""

instance {-# OVERLAPPABLE #-} (Records.HasField name (Responses res) a) => Records.HasField name (Responses ('RESPONSE name' status headers content result ': res)) a where
    getField (Response r) = Records.getField @name r

instance {-# OVERLAPPABLE #-} (Records.HasField name (Responses res) a) => Records.HasField name (Responses ('NOCONTENT name' headers ': res)) a where
    getField (NoContent r) = Records.getField @name r

----------------------
-- RESPONSE HEADERS --
----------------------

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

class ToWaiResponseHeaders (headerKeys :: [TypeLits.Symbol]) where
    toWaiResponseHeaders :: ResponseHeaders headerKeys -> HTTP.ResponseHeaders

instance ToWaiResponseHeaders '[] where
    toWaiResponseHeaders _ = []

instance (ToWaiResponseHeaders headerKeys) => ToWaiResponseHeaders (headerKey ': headerKeys) where
    toWaiResponseHeaders (InsertHeader v tail) = [(CI.mk . Char8.pack $ TypeLits.symbolVal @headerKey Typeable.Proxy, Web.toHeader v)]

-------------------
-- RESPONSE BODY --
-------------------

data ResponseBody
    = ResponseBodyStream Wai.StreamingBody
    | ResponseBodyBuilder Builder.Builder
    | ResponseBodyBytes LBS.ByteString
    | ResponseBodyFile FilePath (Maybe Wai.FilePart)

------------------
-- CONTENT TYPE --
------------------

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

-------------
-- METHOD --
-------------

type METHOD :: Type
data METHOD where
    METHOD :: TypeLits.Symbol -> [Type] -> (Type -> Type) -> VERB -> [RESPONSE] -> METHOD

type MapSub :: Type -> [METHOD] -> [METHOD]
type family MapSub e xs where
    MapSub e '[] = '[]
    MapSub e ('METHOD name path env verb res ': xs) = 'METHOD name (e ': path) env verb res ': MapSub e xs

data Methods (ms :: [METHOD]) where
    None :: Methods '[]
    AddMethod ::
        forall name path env verb res tail.
        (ToStdMethod verb, BuildHandler res path env, URLBuilder path) =>
        Method_ name verb env res path ->
        Methods tail ->
        Methods ('METHOD name path env verb res : tail)

none :: Methods '[]
none = None

addMethod ::
    forall name path env verb res tail.
    (ToStdMethod verb, BuildHandler res path env, URLBuilder path) =>
    Method_ name verb env res path ->
    Methods tail ->
    Methods ('METHOD name path env verb res : tail)
addMethod = AddMethod

appendMethods :: Methods ms -> Methods ms' -> Methods (ms :<> ms')
appendMethods None ys = ys
appendMethods (AddMethod method_ tail) ys = AddMethod method_ (appendMethods tail ys)

class TreeToMethods (t :: TREE) where
    type TTMType t :: [METHOD]
    treeToMethods :: Tree t p -> Methods (TTMType t)

instance (ToStdMethod verb, BuildHandler res path env, URLBuilder path) => TreeToMethods (LEAF name path env verb res) where
    type TTMType (LEAF name path env verb res) = 'METHOD name path env verb res ': '[]
    treeToMethods (Method trans responseBuilder handler) = AddMethod (Method_ @name @verb @env @res @path trans responseBuilder handler) None

instance (TreeToMethods c) => TreeToMethods (NODE n c) where
    type TTMType (NODE _ c) = TTMType c
    treeToMethods (Lit child) = treeToMethods child
    treeToMethods (Param child) = treeToMethods child
    treeToMethods (Splat child) = treeToMethods child

instance (TreeToMethods t, TreeToMethods t') => TreeToMethods (BRANCH t t') where
    type TTMType (BRANCH t t') = TTMType t :<> TTMType t'
    treeToMethods (Branch t t') = treeToMethods t `appendMethods` treeToMethods t'

instance Records.HasField name (Methods ('METHOD name path env verb res ': rs)) (Method_ name verb env res path) where
    getField (AddMethod method_ _) = method_

instance {-# OVERLAPS #-} (Records.HasField name (Methods ms) (Method_ name verb env res path)) => Records.HasField name (Methods ('METHOD name' path' env' verb' res' ': ms)) (Method_ name verb env res path) where
    getField (AddMethod _ tail) = Records.getField @name tail

-------------
-- HANDLER --
-------------

data Env res = Env
    { request :: Wai.Request
    , responses :: Responses res
    }

type Handler :: [RESPONSE] -> [Type] -> (Type -> Type) -> Type
type family Handler res p env where
    Handler res '[] env = Env res -> env Wai.Response
    Handler res (Lit_ s : rem) env = Handler res rem env
    Handler res (Param_ a : rem) env = a -> Handler res rem env
    Handler res (Splat_ a : rem) env = NonEmpty.NonEmpty a -> Handler res rem env
    Handler res x _ = TypeError.TypeError (TypeError.Text "Can't create Handler for type: " TypeError.:<>: TypeError.ShowType x)

class BuildHandler res args env where
    buildHandler :: Handler res args env -> HList args -> (Env res -> env Wai.Response)

instance BuildHandler res '[] env where
    buildHandler handler HNil = handler

instance (BuildHandler res xs env) => BuildHandler res (Lit_ s : xs) env where
    buildHandler handler (HCons _ xs) = buildHandler @res @xs @env handler xs

instance (BuildHandler res xs env) => BuildHandler res (Param_ a : xs) env where
    buildHandler handler (HCons (Param_ x) xs) = buildHandler @res @xs @env (handler x) xs

instance (BuildHandler res xs env) => BuildHandler res (Splat_ a : xs) env where
    buildHandler handler (HCons (Splat_ nel) xs) = buildHandler @res @xs @env (handler nel) xs

-----------------------
-- Heterogenous List --
-----------------------

data HList (types :: [Type]) where
    HNil :: HList '[]
    HCons :: x -> HList xs -> HList (x : xs)

snoc :: a -> HList p -> HList (p :< a)
snoc x HNil = HCons x HNil
snoc x (HCons h t) = HCons h (snoc x t)

-------------------------
-- Tree --> Middleware --
-------------------------

app :: Root t -> Wai.Middleware
app = appHelper HNil

appHelper :: HList args -> Tree t args -> Wai.Middleware
appHelper args tree backup request respond = case tree of
    (Method @_ @v trans responses handler) -> do
        case HTTP.parseMethod request.requestMethod of
            Left _ -> backup request respond
            Right stdMethod ->
                if toStdMethod @v == stdMethod
                    then do
                        result <- trans $ buildHandler handler args $ Env request (responses Nil)
                        respond result
                    else backup request respond
    (Param @param child) -> case request.pathInfo of
        (ph : pt) ->
            case Web.parseUrlPiece @param ph of
                Left _ -> backup request respond
                Right param -> do
                    let newRequest = request{Wai.pathInfo = pt}
                    appHelper (snoc (Param_ @param param) args) child backup newRequest respond
        [] -> backup request respond
    (Lit @s child) -> case request.pathInfo of
        (ph : pt) ->
            if Text.pack (TypeLits.symbolVal @s Typeable.Proxy) == ph
                then do
                    let newRequest = request{Wai.pathInfo = pt}
                    appHelper (snoc (Lit_ @s) args) child backup newRequest respond
                else backup request respond
        [] -> backup request respond
    (Splat @param child) -> case request.pathInfo of
        (ph : pt) ->
            case Web.parseUrlPiece @param ph of
                Left _ -> backup request respond
                Right param -> case pt of
                    [] -> appHelper (snoc (Splat_ (param NonEmpty.:| [])) args) child backup (request{Wai.pathInfo = pt}) respond
                    pt' ->
                        let (params, newPathInfo) = forSplat @param ([], pt')
                         in appHelper (snoc (Splat_ (param NonEmpty.:| params)) args) child backup (request{Wai.pathInfo = newPathInfo}) respond
        [] -> backup request respond
    (Branch tree1 tree2) -> appHelper args tree1 (appHelper args tree2 backup) request respond

forSplat :: (Web.FromHttpApiData a) => ([a], [Text.Text]) -> ([a], [Text.Text])
forSplat (accum, []) = (accum, [])
forSplat (accum, h : t) = case Web.parseUrlPiece h of
    Left _ -> (accum, h : t)
    Right param -> forSplat (param : accum, t)

natToStatus :: Nat.Nat -> HTTP.Status
natToStatus n = toEnum $ fromEnum n

------------------------------------------
-- TYPE FAMILIES (Type-level Functions) --
------------------------------------------

type (:<>) :: [k] -> [k] -> [k]
type family (:<>) xs ys where
    '[] :<> ys = ys
    (x : xs) :<> ys = x : xs :<> ys

type (:<) :: [k] -> k -> [k]
type family (:<) xs x where
    '[] :< x = x : '[]
    (x : xs) :< x' = x : (xs :< x')

type Valid :: TREE -> Bool
type family Valid api where
    Valid t = ValidHelper '[] '[] t

type ValidHelper :: [Type] -> [VERB] -> TREE -> Bool
type family ValidHelper nodes leaves t where
    ValidHelper seenNodes seenLeaves (LEAF _ _ _ v _) = NotElem v seenLeaves
    ValidHelper seenNodes seenLeaves (NODE n st) =
        NotElem n seenNodes && Valid st
    ValidHelper seenNodes seenLeaves (BRANCH (LEAF _ _ _ v _) t') =
        NotElem v seenLeaves && ValidHelper seenNodes (v : seenLeaves) t'
    ValidHelper seenNodes seenLeaves (BRANCH (NODE n st) t') =
        NotElem n seenNodes && Valid st && ValidHelper (n : seenNodes) seenLeaves t'
    ValidHelper seenNodes seenLeaves (BRANCH t t') =
        ValidHelper seenNodes seenLeaves t && ValidHelper seenNodes seenLeaves t'

type NotElem :: k -> [k] -> Bool
type family NotElem k ks where
    NotElem k '[] = True
    NotElem k (k : ks) = False
    NotElem k (k' : ks) = True && NotElem k ks

----------
-- TEST --
----------

runApi = do
    print "Running app..."
    Warp.run 3000 $ app api $ \request respond -> respond $ Wai.responseLBS (toEnum 404) [] "Not Found"

api :: Root _
api = home homeHandler ||| person personHandler ||| (lit @"new" . param @Text.Text $ method @"newPerson" @POST @IO id homeResponses \_ -> undefined)

home =
    lit @"hello"
        . lit @"world"
        . param @Text.Text
        . param @Float
        . method @"home" @GET @IO id homeResponses

homeResponses =
    response @"ok" @200 @'[] @Text.Text @Text.Text
        . response @"error" @500 @'[] @Text.Text @Text.Text

homeHandler (name :: Text.Text) (age :: Float) env =
    if name == "Bob"
        then return $ env.responses.ok noHeaders "Hello"
        else return $ env.responses.error noHeaders "Bye"

person = lit @"person" . method @"putPerson" @PUT @IO id personResponses

personResponses = response @"ok" @200 @'[] @Text.Text @Text.Text . noContent @"none" @'[]

personHandler env = return $ env.responses.none noHeaders

test = api.putPerson
