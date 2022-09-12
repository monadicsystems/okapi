{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Data.Text
import Okapi

data Todo = Todo
  { todoID :: Int
  , todoText :: Text
  }
  
newtype TodoForm = TodoForm { todoFormText :: Text }

pattern GetTodos = (GET, ["todos"])

pattern GetTodo tID = (GET, ["todos", PathParam tID])

pattern PostTodo = (POST, ["todos"])

pattern PatchTodo tID = (PATCH, ["todos", PathParam tID])

pattern DeleteTodo tID = (DELETE, ["todos", PathParam tID])

methodAndPath :: MonadOkapi m => m (Method, Path)
methodAndPath = do
  m <- method
  p <- path
  return (m, p)
  
renderRedirect :: (Method, Path) -> Text
renderRedirect (_, p) = renderPath p

renderFormAction :: (Method, Path) -> Text
renderFormAction (m, p) = undefined

main :: IO ()
main = run id $ route methodAndPath $ \case
  GetTodos -> do
    todos <- liftIO $ queryAllTodos
    return $ setJSON todos $ ok
  GetTodo tID -> do
    maybeTodo <- liftIO $ queryTodo tID
    case maybeTodo of
      Nothing   -> return notFound
      Just todo -> return $ setJSON todo $ ok
  PostTodo -> do
    todoForm <- bodyForm @TodoForm
    maybeNewTodo <- liftIO $ insertTodoForm todoForm
    case maybeNewTodo of
      Nothing      -> return notFound
      Just newTodo -> return $ setJSON newTodo $ ok
  PatchTodo tID -> do
    todoForm <- bodyForm @TodoForm
    maybePatchedTodo <- liftIO $ updateTodo tID todoForm
    case maybePatchedTodo of
      Nothing      -> return notFound
      Just patchedTodo -> return $ setJSON patchedTodo $ ok
  DeleteTodo tID -> do
    maybeDeleted <- liftIO $ deleteTodo tID
    case maybeDeleted of
      Nothing -> return notFound
      Just _  -> redirect $ renderRedirect GetTodos
  _ -> next
