{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Lazy.Char8 qualified as LBS8
import Data.OpenApi (ToSchema)
import Data.Text (Text)
import GHC.Generics (Generic)
import Network.HTTP.Types qualified as Types
import Network.HTTP.Client qualified as HC
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Okapi.Record.Data qualified as Data

import Okapi

data Message = Message
    { temperature :: Integer
    , model :: Text
    , messages :: [InnerMessage]
    , max_tokens :: Integer
    }
    deriving (Generic, Aeson.FromJSON, Aeson.ToJSON, ToSchema)

data InnerMessage = InnerMessage
    { content :: Text
    , role :: Text
    }
    deriving (Generic, Aeson.FromJSON, Aeson.ToJSON, ToSchema)

createMessageReq = reqPOST
    { path = do
        seg_ text "v1"
        seg_ text "messages"
        pure ()
    , headers = do
        contentType JSON
        field_ "X-Api-Key" "<API_KEY>"
        field_ "anthropic-version" "2023-06-01"
        maybeUserProfileId <- field' "anthropic-user-profile-id" text
        pure maybeUserProfileId
    , body = json @Message
    }

data CreateMessageResponses f
    = Created (f (KnownStatus 201) Types.ResponseHeaders (IO LBS.ByteString))
    | Any (f Types.Status Types.ResponseHeaders (IO LBS.ByteString))
    deriving (Generic, Cases)

createMessageRes = res201

createMessageContract = createMessageReq :-< cases @CreateMessageResponses
    res201
    res

-- | Renders 'createMessageContract' as an OpenAPI document — only needs the
--   'Contract' itself, no handler, so this is callable straight from the
--   repl: @cabal repl okapi@, @:load test\/Ex1.hs@, then @printSchema@.
printSchema :: IO ()
printSchema = LBS8.putStrLn (Aeson.encode (contractToOpenApi createMessageContract))

-- | Entry point for @cabal run okapi-ex1@ — deliberately just the safe,
--   no-network 'printSchema', not 'createMessage' (which fires a real
--   request against the live Anthropic API using whatever credential
--   'createMessageReq' is holding). Load this module in the repl instead
--   (see 'createMessage'\'s haddock) to call that by hand.
main :: IO ()
main = printSchema

anthropicSettings :: IO ClientSettings
anthropicSettings = do
    mgr <- HC.newManager tlsManagerSettings
    pure ClientSettings { manager = mgr, baseUrl = "https://api.anthropic.com" }

-- | Callable from the repl once you have a request value built:
--   @cabal repl okapi-ex1@, @:load test\/Ex1.hs@, then
--   @createMessage someRequestValue@.
createMessage reqVal = do
    settings <- anthropicSettings
    case clientFor settings createMessageContract of
        Fn f -> f reqVal

lookAtResult :: Either ClientError (CreateMessageResponses Data.Response) -> IO ()
lookAtResult cmr = case cmr of
    Left _ -> print "errored"
    Right aResp -> case aResp of
        Created _ -> print 201
        Any resData -> do
            print resData.status
            print resData.headers
            bodyResult <- resData.body
            print bodyResult

testRequest = (Data.Request POST () [] Nothing ((pure $ Message 1 "claude-opus-4-6" [InnerMessage "h1!" "user"] 1024) :: IO Message))
