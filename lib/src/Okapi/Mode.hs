{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Okapi.Mode where

import Data.Kind (Type)
import Network.Wai qualified as Wai
import Okapi.Codec (IsoCodec, Value)
import Okapi.Req (Req)
import Okapi.ResAlt (ResAlt)

-- foldCodec ::
--     (Profunctor f, forall x. Applicative (f x)) =>
--     (forall i o. t i o -> f i o) ->
--     Codec t i o ->
--     f i o
-- foldCodec alg = \case
--     FMap f c -> fmap f (foldCodec alg c)
--     LMap f c -> lmap f (foldCodec alg c)
--     Pure x -> pure x
--     Apply f x -> foldCodec alg f <*> foldCodec alg x
--     Embed t -> alg t

data
    Signature
        (m :: Type)
        (p :: Type)
        (q :: Type)
        (h :: Type)
        (b :: Type)
        (r :: ((Type -> Type) -> Type -> Type) -> Type)

{-
r needs to be higher kinded because it needs to be a sum type where every constructor
is a Response, and Response needs to take the Codec or Value tag:

type MyResponses :: ((Type -> Type) -> Type -> Type) -> Type
data MyResponses f
  = OK (Response f S200 ...)
  | NotFound (Response f S404 ...)
-}

data ClientError

data Endpoint sig where
    (:->) ::
        Req IsoCodec m p q h b ->
        IsoCodec ResAlt (r Value) ->
        Endpoint (Signature m p q h b r)

data Client sig where
    Cb :: -- Callback
        (Req Value m p q h b -> IO (Either ClientError (r Value))) ->
        Client (Signature m p q h b r)

data Server n sig where
    Fn :: -- Function
        ((Req Value m p q h b, Wai.Request) -> n (r Value)) ->
        Server n (Signature m p q h b r)

fn ::
    ((Req Value m p q h b, Wai.Request) -> n (r Value)) ->
    Server n (Signature m p q h b r)
fn = Fn

-- EXAMPLE AREA
-- Response types
{-
data GetUserResponses f
    = GetUserOK       (Response f S200 HTTP.ResponseHeaders LBS.ByteString)
    | GetUserNotFound (Response f S404 HTTP.ResponseHeaders LBS.ByteString)
    deriving (Generic, GenericResponses)

data CreateUserResponses f
    = CreateUserOK    (Response f S200 HTTP.ResponseHeaders LBS.ByteString)
    | CreateUserError (Response f S500 HTTP.ResponseHeaders LBS.ByteString)
    deriving (Generic, GenericResponses)

-- Signature type synonyms
type GetUserEndpoint = Signature
    HTTP.StdMethod
    UserId
    ()
    HTTP.RequestHeaders
    (IO LBS.ByteString)
    GetUserResponses

type CreateUserEndpoint = Signature
    HTTP.StdMethod
    ()
    ()
    HTTP.RequestHeaders
    (IO CreateUserBody)
    CreateUserResponses

-- API record
data UsersApi mode = UsersApi
    { getUser    :: mode GetUserEndpoint
    , createUser :: mode CreateUserEndpoint
    }

-- Codecs
usersCodecs :: UsersApi Endpoint
usersCodecs = UsersApi
    { getUser    = getUserRequest    :-> getUserResponses
    , createUser = createUserRequest :-> createUserResponses
    }

-- Handlers
usersServer :: UsersApi (Server IO)
usersServer = UsersApi
    { getUser = fn \(req, _) -> do
        user <- DB.fetchUser req.path.value
        pure (GetUserOK user)
    , createUser = fn \(req, _) -> do
        body   <- req.reqBody.value
        result <- DB.insertUser body
        pure (CreateUserOK result)
    }

data MyResponses f
    = OK       (Response f S200 HTTP.ResponseHeaders LBS.ByteString)
    | NotFound (Response f S404 HTTP.ResponseHeaders LBS.ByteString)
    | Error    (Response f S500 HTTP.ResponseHeaders LBS.ByteString)
    deriving (Generic, GenericResponses)

usersProductsPath = do
    Path.lit "users"
    userId    <- fst .= Path.param @UserId
    productId <- snd .= Path.param @ProductId
    pure (userId, productId)

myRequest
    = request
    . path' do -- /users/:userID/:productID/blah
        (userId, productId) <- usersProductsPath
        Path.lit "blah"
        pure (userId, productId)
    . query' do
        age <- Query.param @Int "age"
        Query.flag "male"
        pure age

myResponses = responses @MyResponses
    okResponse
    notFoundResponse
    errorResponse
    where
        okResponse
            = response
            . status' 200

myRoute = myRequest :-> myResponses

-- servant-named-routes
data MyApplication f = MyApplication
    { getUsers :: f ()
    , postUsers :: f ()
    , deleteUsers :: f ()
    }

-}
