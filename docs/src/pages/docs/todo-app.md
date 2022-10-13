---
title: Todo app
description: A tutorial on building a todo app.
---

A tutorial on building a todo app that covers the basics of Okapi.

---

## Todo app

The goal for this tutorial is to build a JSON API with the following endpoints:

- `GET /`

  Health check. Returns 200 response with "OK" as the body.

- `GET /todos`

  Returns all todos as JSON.

- `POST /todos`

  Accepts a todo/todos encoded as JSON to store on the server.

- `GET /todos/:uid`

  Returns the todo with the matching `:uid` as JSON.

- `PATCH /todos/:uid`

  Accepts todo information as JSON. For updating a todo with the `:uid`.

- `DELETE /todos/:uid`

  Deletes the todo with the matching `:uid` from the server.

### Project setup

Make you sure you have the Haskell package manager, `stack`, installed before you continue.

To create the project folder, run the following command in your terminal:

```bash
stack new todo
```

This will create a directory called `todo` with the following contents:

```
- src
- package.yaml
- stack.yaml
...
```

Our project is going to depend on three libraries: `okapi` for implementing the required API endpoints,
 `direct-sqlite` for persisting/reading data, and `text` for handling values of the `Text` type.

Go to the `package.yaml` file in your new `todo` folder, and add the following lines under `dependencies:`:

```yaml
- okapi
- direct-sqlite
- text
```

Save and close the file, then run the `stack build` command to make sure your project builds.

### Implementation

We're going to implement everything in `Main.hs` to keep it simple. The entry point to the app is going
to be `main :: IO ()`.

```haskell
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Applicative ((<|>))
import Control.Applicative.Combinators
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Data.Aeson (ToJSON, toJSON)
import Data.ByteString.Lazy (fromStrict)
import Data.Function ((&))
import Data.Maybe (listToMaybe)
import Data.Text
import Data.Text.Encoding (encodeUtf8)
import Database.SQLite.Simple
import Database.SQLite.Simple.FromField
import Database.SQLite.Simple.ToField
import GHC.Generics (Generic, Par1)
import Okapi
import Web.FormUrlEncoded (FromForm)
import Web.HttpApiData (ToHttpApiData)
import Web.Internal.HttpApiData

-- TYPES --

data Todo = Todo
  { todoID :: Int,
    todoName :: Text,
    todoStatus :: TodoStatus
  }
  deriving (Eq, Ord, Generic, ToJSON, Show)

instance FromRow Todo where
  fromRow = Todo <$> field <*> field <*> field

data TodoForm = TodoForm
  { todoFormName :: Text,
    todoFormStatus :: TodoStatus
  }
  deriving (Eq, Ord, Generic, FromForm, Show)

instance ToRow TodoForm where
  toRow (TodoForm name status) = toRow (name, status)

data TodoStatus
  = Incomplete
  | Archived
  | Complete
  deriving (Eq, Ord, Show)

instance ToJSON TodoStatus where
  toJSON Incomplete = "incomplete"
  toJSON Archived = "archived"
  toJSON Complete = "complete"

instance FromHttpApiData TodoStatus where
  parseQueryParam "incomplete" = Right Incomplete
  parseQueryParam "archived" = Right Archived
  parseQueryParam "complete" = Right Complete
  parseQueryParam _ = Left "Incorrect format for TodoStatus value"

instance ToField TodoStatus where
  toField status =
    case status of
      Incomplete -> SQLText "incomplete"
      Archived -> SQLText "archived"
      Complete -> SQLText "complete"

instance FromField TodoStatus where
  fromField field = do
    case fieldData field of
      SQLText "incomplete" -> pure Incomplete
      SQLText "archived" -> pure Archived
      SQLText "complete" -> pure Complete
      _ -> returnError ConversionFailed field "Couldn't methodGET TodoStatus value from field"

type Okapi = ServerT IO

-- MAIN --

main :: IO ()
main = do
  conn <- open "todo.db"
  execute_ conn "CREATE TABLE IF NOT EXISTS todos (id INTEGER PRIMARY KEY, name TEXT, status TEXT)"
  run id (todoAPI conn)
  close conn

-- SERVER FUNCTIONS

respond :: Response -> Okapi Response
respond response = do
  methodEnd
  pathEnd
  return response

todoAPI :: Connection -> Okapi Response
todoAPI conn =
  healthCheck
    <|> getTodo conn
    <|> getAllTodos conn
    <|> createTodo conn
    <|> editTodo conn
    <|> forgetTodo conn

healthCheck :: Okapi Response
healthCheck = do
  methodGET
  optional $ pathParam @Text `is` ""
  respond ok

getTodo :: Connection -> Okapi Response
getTodo conn = do
  methodGET
  pathParam @Text `is` "todos"
  todoID <- pathParam @Int
  maybeTodo <- lift $ selectTodo conn todoID
  case maybeTodo of
    Nothing -> throw internalServerError
    Just todo -> ok & setJSON todo & respond

getAllTodos :: Connection -> Okapi Response
getAllTodos conn = do
  methodGET
  pathParam @Text `is` "todos"
  status <- optional $ queryParam @TodoStatus "status"
  todos <- lift $ selectAllTodos conn status
  ok & setJSON todos & respond

createTodo :: Connection -> Okapi Response
createTodo conn = do
  methodPOST
  pathParam @Text `is` "todos"
  todoForm <- bodyURLEncoded
  lift $ insertTodoForm conn todoForm
  respond ok

editTodo :: Connection -> Okapi Response
editTodo conn = do
  methodPUT
  is @Text pathParam "todos"
  todoID <- pathParam @Int
  todoForm <- bodyURLEncoded @TodoForm
  lift $ updateTodo conn todoID todoForm
  respond ok

forgetTodo :: Connection -> Okapi Response
forgetTodo conn = do
  methodDELETE
  pathParam @Text `is` "todos"
  todoID <- pathParam @Int
  lift $ deleteTodo conn todoID
  respond ok

-- DATABASE FUNCTIONS

insertTodoForm :: Connection -> TodoForm -> IO ()
insertTodoForm conn = execute conn "INSERT INTO todos (name, status) VALUES (?, ?)"

selectTodo :: Connection -> Int -> IO (Maybe Todo)
selectTodo conn todoID = listToMaybe <$> Database.SQLite.Simple.query conn "SELECT * FROM todos WHERE id = ?" (Only todoID)

selectAllTodos :: Connection -> Maybe TodoStatus -> IO [Todo]
selectAllTodos conn maybeStatus = case maybeStatus of
  Nothing -> query_ conn "SELECT * FROM todos"
  Just status -> Database.SQLite.Simple.query conn "SELECT * FROM todos WHERE status = ?" (Only status)

updateTodo :: Connection -> Int -> TodoForm -> IO ()
updateTodo conn todoID TodoForm {..} =
  executeNamed
    conn
    "UPDATE todos SET name = :name, status = :status WHERE id = :id"
    [":id" := todoID, ":name" := todoFormName, ":status" := todoFormStatus]

deleteTodo :: Connection -> Int -> IO ()
deleteTodo conn todoID = execute conn "DELETE FROM todos WHERE id = ?" (Only todoID)
```
