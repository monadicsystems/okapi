{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)
import Okapi.Protocol.Shared.Body qualified as Body
import System.Exit (exitFailure)
import Web.FormUrlEncoded (FromForm (..), ToForm (..), parseUnique)

assertEq :: (Show a, Eq a) => String -> a -> a -> IO ()
assertEq name expected actual
    | expected == actual = putStrLn ("PASS: " ++ name)
    | otherwise = do
        putStrLn ("FAIL: " ++ name)
        putStrLn ("  expected: " ++ show expected)
        putStrLn ("  actual:   " ++ show actual)
        exitFailure

-- flat record: Generic ToForm/FromForm
data Profile = Profile { username :: Text, age :: Int }
    deriving (Generic, Eq, Show)
instance ToForm Profile
instance FromForm Profile

-- Stripe-style nested form: hand-written instances emit bracketed keys
data CreateSession = CreateSession
    { csMode  :: Text
    , csPrice :: Text
    , csQty   :: Int
    } deriving (Eq, Show)

instance ToForm CreateSession where
    toForm cs =
        [ ("mode", csMode cs)
        , ("line_items[0][price]", csPrice cs)
        , ("line_items[0][quantity]", T.pack (show (csQty cs)))
        ]

instance FromForm CreateSession where
    fromForm f = CreateSession
        <$> parseUnique "mode" f
        <*> parseUnique "line_items[0][price]" f
        <*> parseUnique "line_items[0][quantity]" f

-- run a value through the form body codec: print to bytes, then parse back
roundTrip :: (ToForm a, FromForm a) => a -> IO (Either Body.ParseError a, LBS.ByteString)
roundTrip val = do
    bytes <- Body.printM Body.form (pure val)
    case Body.parse Body.form bytes of
        (Left e, _)        -> pure (Left e, bytes)
        (Right ioVal, _)   -> do
            v <- ioVal
            pure (Right v, bytes)

main :: IO ()
main = do
    -- ── flat Generic form round-trips ─────────────────────────────────────────
    let p = Profile { username = "alice", age = 30 }
    (rp, _) <- roundTrip p
    assertEq "flat form round-trip" (Right p) rp

    -- ── nested Stripe-style form ──────────────────────────────────────────────
    let cs = CreateSession { csMode = "payment", csPrice = "price_123", csQty = 2 }
    (rcs, bytes) <- roundTrip cs
    assertEq "nested form round-trip" (Right cs) rcs
    -- the printed body carries Stripe's bracketed keys (URL-encoded)
    assertEq "nested form emits line_items[0][price]"
        True
        (BS.isInfixOf "line_items%5B0%5D%5Bprice%5D=price_123" (LBS.toStrict bytes))
    assertEq "nested form emits line_items[0][quantity]"
        True
        (BS.isInfixOf "line_items%5B0%5D%5Bquantity%5D=2" (LBS.toStrict bytes))

    putStrLn "all form-body tests passed"
