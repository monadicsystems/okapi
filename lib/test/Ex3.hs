{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Main where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader, ReaderT, asks, runReaderT)
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as LBS
import Data.Function ((&))
import Data.Int (Int64)
import Data.Maybe (listToMaybe)
import Data.OpenApi (ToSchema)
import Data.Text (Text)
import Database.SQLite.Simple qualified as SQLite
import GHC.Generics (Generic)
import Network.HTTP.Client qualified as HC
import Network.HTTP.Types qualified as HTTP
import Network.Wai.Handler.Warp qualified as Warp
import Okapi
import System.Exit (exitFailure)

-- ── Domain types ─────────────────────────────────────────────────────────────

data Item = Item
    { itemId :: Int64
    , itemName :: Text
    }
    deriving (Generic, Aeson.FromJSON, Aeson.ToJSON, ToSchema)

data NewItem = NewItem
    { itemName :: Text
    }
    deriving (Generic, Aeson.FromJSON, Aeson.ToJSON, ToSchema)

instance SQLite.FromRow Item where
    fromRow = Item <$> SQLite.field <*> SQLite.field

-- ── App monad ────────────────────────────────────────────────────────────────

data Config = Config {dbConn :: SQLite.Connection}

type AppM = ReaderT Config IO

-- ── Database operations ──────────────────────────────────────────────────────

createItemDB :: (MonadReader Config m, MonadIO m) => NewItem -> m Item
createItemDB ni = do
    conn <- asks dbConn
    liftIO $ SQLite.execute conn "INSERT INTO items (name) VALUES (?)" (SQLite.Only ni.itemName)
    rowId <- liftIO $ SQLite.lastInsertRowId conn
    pure Item{itemId = rowId, itemName = ni.itemName}

getItemDB :: (MonadReader Config m, MonadIO m) => Int64 -> m (Maybe Item)
getItemDB iid = do
    conn <- asks dbConn
    rows <- liftIO $ SQLite.query conn "SELECT id, name FROM items WHERE id = ?" (SQLite.Only iid)
    pure (listToMaybe rows)

deleteItemDB :: (MonadReader Config m, MonadIO m) => Int64 -> m Bool
deleteItemDB iid = do
    conn <- asks dbConn
    liftIO $ SQLite.execute conn "DELETE FROM items WHERE id = ?" (SQLite.Only iid)
    n <- liftIO $ SQLite.changes conn
    pure (n > 0)

-- ── Response types ───────────────────────────────────────────────────────────

data GetItemRes f
    = ItemFound (Response f S200 HTTP.ResponseHeaders Item)
    | ItemNotFound (Response f S404 HTTP.ResponseHeaders LBS.ByteString)
    deriving (Generic, GenericResAlt)

data DeleteItemRes f
    = ItemDeleted (Response f S204 HTTP.ResponseHeaders NoContent)
    | DeleteNotFound (Response f S404 HTTP.ResponseHeaders LBS.ByteString)
    deriving (Generic, GenericResAlt)

-- ── Endpoints ────────────────────────────────────────────────────────────────

createItemReq =
    mPost
        & path do
            seg_ @Text "items"
        & body (json @NewItem)

createItemEndpoint =
    createItemReq
        :-> only
            (s201 & body (json @Item))

getItemReq =
    mGet
        & path do
            seg_ @Text "items"
            iid <- seg @Int64 "id"
            pure iid

getItemEndpoint =
    getItemReq
        :-> responsesOf @GetItemRes
            (s200 & body (json @Item))
            s404

deleteItemReq =
    mDelete
        & path do
            seg_ @Text "items"
            iid <- seg @Int64 "id"
            pure iid

deleteItemEndpoint =
    deleteItemReq
        :-> responsesOf @DeleteItemRes
            (s204 & body noContent)
            s404

-- ── HKD API record ───────────────────────────────────────────────────────────

type SigCreate = Signature POST () HTTP.Query HTTP.RequestHeaders NewItem (Only S201 HTTP.ResponseHeaders Item)
type SigGet = Signature GET Int64 HTTP.Query HTTP.RequestHeaders LBS.ByteString GetItemRes
type SigDelete = Signature DELETE Int64 HTTP.Query HTTP.RequestHeaders LBS.ByteString DeleteItemRes

data ItemsApi f = ItemsApi
    { createItem :: f SigCreate
    , getItem :: f SigGet
    , deleteItem :: f SigDelete
    }
    deriving (Generic)

-- ── Endpoints record ─────────────────────────────────────────────────────────

itemsEndpoints :: ItemsApi Contract
itemsEndpoints =
    ItemsApi
        { createItem = createItemEndpoint
        , getItem = getItemEndpoint
        , deleteItem = deleteItemEndpoint
        }

-- ── Handlers ─────────────────────────────────────────────────────────────────

itemsHandlers :: (MonadReader Config m, MonadIO m) => ItemsApi (Server m)
itemsHandlers =
    ItemsApi
        { createItem = fn \(req, _) -> do
            ni <- liftIO req.body_.value
            item <- createItemDB ni
            pure (Only (response S201 [] (pure item)))
        , getItem = fn \(req, _) -> do
            mItem <- getItemDB req.path_.value
            pure $ case mItem of
                Just item -> ItemFound (response S200 [] (pure item))
                Nothing -> ItemNotFound (response S404 [] (pure "item not found"))
        , deleteItem = fn \(req, _) -> do
            deleted <- deleteItemDB req.path_.value
            pure $
                if deleted
                    then ItemDeleted (response S204 [] (pure NoContent))
                    else DeleteNotFound (response S404 [] (pure "item not found"))
        }

-- ── Helpers ──────────────────────────────────────────────────────────────────

check :: String -> Bool -> IO ()
check name True = putStrLn ("PASS: " ++ name)
check name False = putStrLn ("FAIL: " ++ name) >> exitFailure

-- ── Main ─────────────────────────────────────────────────────────────────────

main :: IO ()
main = SQLite.withConnection ":memory:" \conn -> do
    SQLite.execute_
        conn
        "CREATE TABLE items (id INTEGER PRIMARY KEY AUTOINCREMENT, name TEXT NOT NULL)"

    let cfg = Config{dbConn = conn}
        myApp = app itemsEndpoints (flip runReaderT cfg) itemsHandlers

    mgr <- HC.newManager HC.defaultManagerSettings
    Warp.testWithApplication (pure myApp) \port -> do
        let cs = ClientSettings{manager = mgr, baseUrl = "http://localhost:" ++ show port}
            ItemsApi (Cb goCreate) (Cb goGet) (Cb goDelete) = client itemsEndpoints cs

        r1 <- goCreate (request POST () [] [] (pure NewItem{itemName = "Widget"}))
        createdId <- case r1 of
            Right (Only r) -> do
                item <- r.body_.value
                check "POST /items: status" (r.status_.value == S201)
                check "POST /items: name" (item.itemName == "Widget")
                pure item.itemId
            _ -> putStrLn "FAIL: POST /items" >> exitFailure

        r2 <- goGet (request GET createdId [] [] (pure ""))
        case r2 of
            Right (ItemFound r) -> do
                item <- r.body_.value
                check "GET /items/:id: status" (r.status_.value == S200)
                check "GET /items/:id: name" (item.itemName == "Widget")
            _ -> check "GET /items/:id found" False

        r3 <- goGet (request GET 9999 [] [] (pure ""))
        case r3 of
            Right (ItemNotFound r) ->
                check "GET /items/9999: 404" (r.status_.value == S404)
            _ -> check "GET /items/9999 not found" False

        r4 <- goDelete (request DELETE createdId [] [] (pure ""))
        case r4 of
            Right (ItemDeleted r) ->
                check "DELETE /items/:id: status" (r.status_.value == S204)
            _ -> check "DELETE /items/:id deleted" False

        r5 <- goGet (request GET createdId [] [] (pure ""))
        case r5 of
            Right (ItemNotFound r) ->
                check "GET /items/:id after delete: 404" (r.status_.value == S404)
            _ -> check "GET after delete" False

    putStrLn "all ex3 tests passed"
