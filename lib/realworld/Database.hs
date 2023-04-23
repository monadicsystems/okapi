{-# LANGUAGE OverloadedStrings #-}

module Database where

import Control.Applicative
import Data
import qualified Data.Text as T
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow

-- data TestField = TestField Int T.Text deriving (Show)

-- instance FromRow TestField where
--   fromRow = TestField <$> field <*> field

-- instance ToRow TestField where
--   toRow (TestField id_ str) = toRow (id_, str)

-- main :: IO ()
-- main = do
--   conn <- open "test.db"
--   execute_ conn "CREATE TABLE IF NOT EXISTS test (id INTEGER PRIMARY KEY, str TEXT)"
--   execute conn "INSERT INTO test (str) VALUES (?)" (Only ("test string 2" :: String))
--   execute conn "INSERT INTO test (id, str) VALUES (?,?)" (TestField 13 "test string 3")
--   rowId <- lastInsertRowId conn
--   executeNamed conn "UPDATE test SET str = :str WHERE id = :id" [":str" := ("updated str" :: T.Text), ":id" := rowId]
--   r <- query_ conn "SELECT * from test" :: IO [TestField]
--   mapM_ print r
--   execute conn "DELETE FROM test WHERE id = ?" (Only rowId)
--   close conn