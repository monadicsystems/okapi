{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}

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
import Lucid
import Lucid.Htmx

{-
We're going to build a Todo API with the following endpoints:

GET /
Health check. Returns 200 response with "OK" as the body.

GET /todos
Returns all todos as JSON. Has optional query param "status" to filter todos with that status.

GET /todos/:uid
Returns the todo with the matching :uid as JSON.

POST /todos
Accepts a todo/todos encoded as JSON to store on the server.

PATCH /todos/:uid
Accepts todo information as JSON. For updating a todo with the :uid.

DELETE /todos/:uid
Deletes the todo with the matching :uid from the server.
-}

-- TYPES --

data Todo = Todo
  { todoID :: Int,
    todoName :: Text,
    todoStatus :: TodoStatus
  }
  deriving (Eq, Ord, Generic, Show)

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
  | Complete
  deriving (Eq, Ord, Show)

instance FromHttpApiData TodoStatus where
  parseQueryParam "incomplete" = Right Incomplete
  parseQueryParam "complete" = Right Complete
  parseQueryParam _ = Left "Incorrect format for TodoStatus value"

instance ToHttpApiData TodoStatus where
  toQueryParam Incomplete = "incomplete"
  toQueryParam Complete = "complete"

instance ToField TodoStatus where
  toField status =
    case status of
      Incomplete -> SQLText "incomplete"
      Complete -> SQLText "complete"

instance FromField TodoStatus where
  fromField field = do
    case fieldData field of
      SQLText "incomplete" -> pure Incomplete
      SQLText "complete" -> pure Complete
      _ -> returnError ConversionFailed field "Couldn't get TodoStatus value from field"

type Okapi = ServerT IO

-- MAIN --

main :: IO ()
main = do
  conn <- open "todo.db"
  execute_ conn "CREATE TABLE IF NOT EXISTS todos (id INTEGER PRIMARY KEY, name TEXT, status TEXT)"
  run id (todoAPI conn)
  close conn

-- SERVER FUNCTIONS

todoAPI :: (MonadIO m, ServerM m) => Connection -> m Response
todoAPI conn =
  home conn
    <|> getAllTodos conn
    <|> createTodoForm
    <|> createTodo conn
    <|> editTodoStatus conn
    <|> forgetTodo conn
    <|> counter

home :: (ServerM m, MonadIO m) => Connection -> m Response
home conn = do
  methodGET
  pathEnd

  todos <- liftIO $ selectAllTodos conn Nothing

  let html = do
        doctypehtml_ $ do
            head_ [] $ do
                script_ [src_ "https://unpkg.com/htmx.org@1.8.0"] ("" :: Html ())
                -- script_ [src_ "https://cdn.tailwindcss.com"] ("" :: Html ())
            body_ [] $ do
                h1_ "Todo App"
                div_ [id_ "main"] $ do
                    todosTable todos
                    createTodoButton
                    counterHtml 0
  ok |> setLucid html |> return

counter :: ServerM m => m Response
counter = do
  methodGET
  pathParam @Text `is` "counter"
  count <- pathParam @Int
  let newCount = count + 1

  ok |> setLucid (counterHtml newCount) |> return

counterHtml :: Int -> Html ()
counterHtml count = do
  div_ [hxGet_ $ "/counter/" <> tShow count, hxTrigger_ "every 1s", hxSwap_ "outerHTML"] $ (toHtml $ tShow count)

getAllTodos :: (ServerM m, MonadIO m) => Connection -> m Response
getAllTodos conn = do
  methodGET
  pathParam @Text `is` "todos"
  pathEnd
  status <- optional $ queryParam @TodoStatus "status"

  todos <- liftIO $ selectAllTodos conn status

  let html = do
        todosTable todos
        createTodoButton
  ok |> setLucid html |> return

createTodoForm :: ServerM m => m Response
createTodoForm = do
    methodGET
    pathParam @Text `is` "todos"
    pathParam @Text `is` "create"
    pathEnd

    let html = do
            form_ [hxPost_ "/todos", hxTarget_ "#main"] $ do
                label_ [name_ "todoFormName", for_ "todoFormName"] "Todo Name: "
                input_ [name_ "todoFormName", id_ "todoFormName", type_ "text"]
                br_ []
                label_ [name_ "todoStatus", for_ "todoStatus"] "Todo Status: "
                input_ [name_ "todoFormStatus", id_ "todoStatus", type_ "text", value_ "incomplete"]
                button_ [] "Create Todo"

    ok |> setLucid html |> return

createTodo :: (ServerM m, MonadIO m) => Connection -> m Response
createTodo conn = do
  methodPOST
  pathParam @Text `is` "todos"
  pathEnd

  todoForm <- bodyURLEncoded

  liftIO $ insertTodoForm conn todoForm

  todos <- liftIO $ selectAllTodos conn Nothing

  let html = do
        todosTable todos
        createTodoButton
  ok |> setLucid html |> return

editTodoStatus :: (ServerM m, MonadIO m) => Connection -> m Response
editTodoStatus conn = do
  methodPUT
  pathParam @Text `is` "todos" -- /todos
  todoID <- pathParam @Int -- /todos/<todoID>
  pathEnd

  currentStatus <- queryParam @TodoStatus "status"
  let newStatus = case currentStatus of
        Incomplete -> Complete
        Complete -> Incomplete

  liftIO $ updateTodoStatus conn todoID newStatus
  ok |> setLucid (flipTodoStatusButton todoID newStatus) |> return

forgetTodo :: (ServerM m, MonadIO m) => Connection -> m Response
forgetTodo conn = do
  methodDELETE
  pathParam @Text `is` "todos"
  todoID <- pathParam @Int
  pathEnd

  liftIO $ deleteTodo conn todoID

  todos <- liftIO $ selectAllTodos conn Nothing

  let html = do
        todosTable todos
        createTodoButton
  ok |> setLucid html |> return

-- Views

todoRow :: Todo -> Html ()
todoRow Todo{..} = tr_ [] $ do
    td_ [] $ toHtml $ tShow todoID
    td_ [] $ toHtml todoName
    td_ [] $ flipTodoStatusButton todoID todoStatus
    td_ [] $ forgetTodoButton todoID

todosTable :: [Todo] -> Html ()
todosTable [] = h2_ [] $ "You have no todos."
todosTable todos = table_ [] $ mapM_ todoRow todos

flipTodoStatusButton :: Int -> TodoStatus -> Html ()
flipTodoStatusButton todoID todoStatus =
  button_
    [hxPut_ $ "/todos/" <> tShow todoID <> "?status=" <> toQueryParam todoStatus, hxSwap_ "outerHTML"]
    $ case todoStatus of
        Incomplete -> "Complete"
        Complete -> "Incomplete"

forgetTodoButton :: Int -> Html ()
forgetTodoButton todoID =
    button_ [hxDelete_ $ "/todos/" <> tShow todoID, hxTarget_ "#main"] "DELETE"

createTodoButton :: Html ()
createTodoButton = button_ [hxGet_ "/todos/create", hxTarget_ "#main"] "Create Todo"

tShow :: Show a => a -> Text
tShow = pack . show

-- DATABASE FUNCTIONS

insertTodoForm :: MonadIO m => Connection -> TodoForm -> m ()
insertTodoForm conn todoForm = liftIO $ execute conn "INSERT INTO todos (name, status) VALUES (?, ?)" todoForm

selectTodo :: MonadIO m => Connection -> Int -> m (Maybe Todo)
selectTodo conn todoID = liftIO $ listToMaybe <$> Database.SQLite.Simple.query conn "SELECT * FROM todos WHERE id = ?" (Only todoID)

selectAllTodos :: MonadIO m => Connection -> Maybe TodoStatus -> m [Todo]
selectAllTodos conn maybeStatus = liftIO $ case maybeStatus of
  Nothing -> query_ conn "SELECT * FROM todos"
  Just status -> Database.SQLite.Simple.query conn "SELECT * FROM todos WHERE status = ?" (Only status)

updateTodoStatus :: MonadIO m => Connection -> Int -> TodoStatus -> m ()
updateTodoStatus conn todoID todoStatus = liftIO $
  executeNamed
    conn
    "UPDATE todos SET status = :status WHERE id = :id"
    [":id" := todoID, ":status" := todoStatus]

deleteTodo :: MonadIO m => Connection -> Int -> m ()
deleteTodo conn todoID = liftIO $ execute conn "DELETE FROM todos WHERE id = ?" (Only todoID)

-- HELPERS

setLucid :: Html a -> Response -> Response
setLucid html = setHTML $ renderBS html

(|>) = (&)
