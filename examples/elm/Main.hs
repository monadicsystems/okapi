module Main where

import qualified Control.Concurrent as Concurrent
import qualified Control.Monad as Monad
import qualified Control.Monad.Combinators as Combinators
import qualified Control.Monad.IO.Class as IO
import qualified Control.Monad.Logger as Logger
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.IORef as IORef
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import Lucid
import qualified Lucid
import qualified Lucid.Base as Lucid
import qualified Lucid.Htmx as Htmx
import qualified Network.WebSockets as WS
import qualified Okapi
import qualified Okapi.Effect.Failure as Failure
import qualified Okapi.Effect.HTTP as HTTP
import qualified Okapi.Effect.Request.Body as Body
import qualified Okapi.Effect.Request.Headers as Headers
import qualified Okapi.Effect.Request.Method as Method
import qualified Okapi.Effect.Request.Method as Request.Effect
import qualified Okapi.Effect.Request.Path as Path
import qualified Okapi.Effect.Request.Path as Request.Effect
import qualified Okapi.Effect.Request.Query as Query
import qualified Okapi.Effect.Response as Response
import qualified Okapi.Pattern as Pattern
import Okapi.Type.HTTP (HTTPT (..))
import qualified Okapi.Type.Request as Request
import qualified Okapi.Type.Response as Response

main :: IO ()
main = do
  print "Starting now"
  modelRef <- IORef.newIORef ["Make breakfast"]
  -- Okapi.run id $ makeTEAParams modelRef exampleTEA3
  Okapi.serveWebsockets WS.defaultConnectionOptions streamServerApp 3000 Response.notFound id do
    Method.methodGET
    Path.pathPart "start"
    Response.setHTML $ Lucid.renderBS do
      Lucid.doctypehtml_ do
        head_ [] do
          script_ [src_ "https://unpkg.com/htmx.org@1.8.4"] ("" :: Lucid.Html ())
          script_ [src_ "https://unpkg.com/htmx.org/dist/ext/ws.js"] ("" :: Lucid.Html ())
          script_ [src_ "https://cdn.tailwindcss.com?plugins=forms"] ("" :: Lucid.Html ())
        body_ [] do
          div_ [id_ "connector", Htmx.hxExt_ "ws", Lucid.makeAttributes "ws-connect" "/foo"] do
            div_ [id_ "events"] do
              form_ [id_ "", Lucid.makeAttributes "ws-send" ""] do
                input_ [type_ "text", name_ "someText"]

streamServerApp :: WS.ServerApp
streamServerApp pendingConn = do
  -- Check path before accepting request
  conn <- WS.acceptRequest pendingConn
  Monad.forever $ eventLoop conn

eventLoop :: WS.Connection -> IO ()
eventLoop conn = do
  message :: LBS.ByteString <- WS.receiveData conn
  case Aeson.decode message of
    Nothing -> do
      print message
      WS.sendTextData conn $ "<div id=\"events\" hx-swap-oob=\"beforeend\"><h1>Couldn't parse message.</h1><p>" <> message <> "</p></div>"
    Just (value :: Aeson.Value) -> do
      print value
      WS.sendTextData conn $ "<div id=\"events\" hx-swap-oob=\"beforeend\"><p>" <> message <> "</p></div>"

instance Logger.MonadLogger IO where
  monadLoggerLog ::
    Logger.ToLogStr msg =>
    Logger.Loc ->
    Logger.LogSource ->
    Logger.LogLevel ->
    msg ->
    IO ()
  monadLoggerLog _ _ _ msg = print (Logger.fromLogStr $ Logger.toLogStr msg)

type Msg = (Request.Method, Request.Path)

data Trigger
  = Trigger {event :: Event, modifiers :: [Modifier], filters :: [Filter]}
  | Intersect {intersectModifiers :: [IntersectModifier], filters :: [Filter]}

click :: [Modifier] -> [Filter] -> Trigger
click = Trigger Click

data Event
  = Click
  | MouseEnter
  | Keyup
  | Load
  | Every Int

data Modifier
  = Once
  | Delay Int
  | Changed

data IntersectModifier = Root Text.Text | Threshold Float | Modifier Modifier

data Filter
  = Custom Text.Text
  | CtrlKey

-- For now just use Text
data Target = Target Text.Text

data Swap = Inner | Outer | AfterBegin | BeforeBegin | BeforeEnd | AfterEnd | None

-- TODO: change text to target/css selector
on :: Trigger -> Msg -> Text.Text -> Swap -> [Lucid.Attributes]
on trigger msg target swap =
  [ triggerToHxTrigger trigger,
    msgToHxVerb msg,
    targetToHxTarget target,
    swapToHxSwap swap
  ]
  where
    triggerToHxTrigger :: Trigger -> Lucid.Attributes
    triggerToHxTrigger (Trigger event [] []) = case event of
      Click -> Htmx.hxTrigger_ "click"
      _ -> mempty

    msgToHxVerb :: Msg -> Lucid.Attributes
    msgToHxVerb (method', path') =
      let url = Pattern.renderPath path'
       in case method' of
            Request.GET -> Htmx.hxGet_ url
            Request.POST -> Htmx.hxPost_ url
            Request.DELETE -> Htmx.hxDelete_ url
            _ -> Htmx.hxGet_ url

    targetToHxTarget :: Text.Text -> Lucid.Attributes
    targetToHxTarget = Htmx.hxTarget_

    swapToHxSwap :: Swap -> Lucid.Attributes
    swapToHxSwap = \case
      Outer -> Htmx.hxSwap_ "outerHTML"
      Inner -> Htmx.hxSwap_ "innerHTML"
      _ -> Htmx.hxSwap_ "outerHTML"

data TEA a = TEA
  { update :: Msg -> a -> a,
    view :: a -> Lucid.Html (),
    wrapper :: Lucid.Html () -> Lucid.Html ()
  }

pattern Increment :: Msg
pattern Increment = (Request.GET, ["increment"])

pattern Decrement :: Msg
pattern Decrement = (Request.GET, ["decrement"])

exampleTEA :: TEA Int
exampleTEA =
  TEA
    { update = \msg model -> case msg of
        Increment -> model + 1
        Decrement -> model - 1
        _ -> model,
      view = \model -> div_ [id_ "counter"] do
        h1_ [] "Counter"
        button_ (on (click [] []) Increment "#counter" Outer) "+"
        div_ [] $ toHtml $ show model
        button_ (on (click [] []) Decrement "#counter" Outer) "-",
      wrapper = \content -> Lucid.doctypehtml_ do
        head_ [] do
          script_ [src_ "https://unpkg.com/htmx.org@1.8.0"] ("" :: Lucid.Html ())
        body_ [] content
    }

exampleTEA2 :: TEA Int
exampleTEA2 =
  TEA
    { update = \msg model -> case msg of
        Increment -> model + 1
        Decrement -> model - 1
        _ -> model,
      view = \model -> do
        h1_ [] "Counter"
        button_ (on (click [] []) Increment "#counter" Inner) "+"
        div_ [] $ toHtml $ show model
        button_ (on (click [] []) Decrement "#counter" Inner) "-",
      wrapper = \content -> Lucid.doctypehtml_ do
        head_ [] do
          script_ [src_ "https://unpkg.com/htmx.org@1.8.0"] ("" :: Lucid.Html ())
        body_ [] $ div_ [id_ "counter"] content
    }

data TEAParams a = TEAParams
  { update :: forall m. (Query.MonadQuery m, Body.MonadBody m) => Msg -> a -> m a,
    view :: a -> Lucid.Html (),
    wrapper :: Lucid.Html () -> Lucid.Html ()
  }

pattern GetTodos :: Msg
pattern GetTodos = (Request.GET, ["todos"])

pattern CreateTodo :: Msg
pattern CreateTodo = (Request.POST, ["todos"])

pattern DeleteTodo :: Msg
pattern DeleteTodo = (Request.DELETE, ["todos"])

exampleTEA3 :: TEAParams [Text.Text]
exampleTEA3 =
  TEAParams
    { update = \msg model -> case msg of
        GetTodos -> pure model
        CreateTodo -> do
          todo <- Body.formParam @Text.Text "todo"
          pure $ model ++ [todo]
        DeleteTodo -> do
          todo <- Body.formParam @Text.Text "todo"
          pure $ List.delete todo model
        _ -> Failure.next,
      view = \model -> div_ [id_ "todoList"] do
        h1_ [] "Todo List"
        div_ [] do
          Monad.forM_ model \todo -> do
            div_ [class_ "flex"] do
              input_
                [ type_ "text",
                  name_ "todo",
                  value_ todo,
                  readonly_ "",
                  class_ ""
                ]
              button_ (on (click [] []) DeleteTodo "#todoList" Outer <> [Htmx.hxInclude_ "[name='todo']"]) ("Delete" :: Lucid.Html ())
          div_ [class_ "flex"] do
            input_
              [ type_ "text",
                name_ "todo",
                id_ "new_todo",
                value_ "",
                class_ ""
              ]
            button_ (on (click [] []) CreateTodo "#todoList" Outer <> [Htmx.hxInclude_ "#new_todo"]) ("Create Todo" :: Lucid.Html ()),
      wrapper = \content -> Lucid.doctypehtml_ do
        head_ [] do
          script_ [src_ "https://unpkg.com/htmx.org@1.8.0"] ("" :: Lucid.Html ())
          script_ [src_ "https://cdn.tailwindcss.com?plugins=forms"] ("" :: Lucid.Html ())
        body_ [] content
    }

makeTEAParams ::
  (HTTP.MonadHTTP m, IO.MonadIO m) =>
  IORef.IORef a ->
  TEAParams a ->
  m ()
makeTEAParams modelRef (TEAParams update view wrapper) = do
  currentModel <- IO.liftIO $ IORef.readIORef modelRef
  m <- Request.Effect.method
  p <- Request.Effect.path
  IO.liftIO $ print p
  let msg = (m, p)
  updatedModel <- update msg currentModel
  IO.liftIO $ IORef.writeIORef modelRef updatedModel
  hxRequestHeaderValue <- Combinators.optional $ Headers.header "HX-Request"
  let html = case hxRequestHeaderValue of
        Just "true" -> view updatedModel
        _ -> wrapper $ view updatedModel
  Response.setHTML $ Lucid.renderBS html

makeTEA ::
  ( Response.MonadResponse m,
    Method.MonadMethod m,
    Path.MonadPath m,
    Headers.MonadHeaders m,
    IO.MonadIO m
  ) =>
  IORef.IORef a ->
  TEA a ->
  m ()
makeTEA modelRef (TEA update view wrapper) = do
  currentModel <- IO.liftIO $ IORef.readIORef modelRef
  m <- Request.Effect.method
  p <- Request.Effect.path
  let msg = (m, p)
      updatedModel = update msg currentModel
  IO.liftIO $ IORef.writeIORef modelRef updatedModel
  hxRequestHeaderValue <- Combinators.optional $ Headers.header "HX-Request"
  let html = case hxRequestHeaderValue of
        Just "true" -> view updatedModel
        _ -> wrapper $ view updatedModel
  Response.setHTML $ Lucid.renderBS html

{-
div []
  [ button [ onClick Decrement ] [ text "-" ]
  , div [] [ text (String.fromInt model) ]
  , button [ onClick Increment ] [ text "+" ]
  ]
-}

-- runTEA
{-
data TEA m a = TEA
    { init :: m a
    , update :: Msg -> m a -> m a
    , view :: a -> Lucid.Html ()
    }
-}

-- data Component m a = Component
--   { view :: a -> Lucid.Html ()
--   ,
--   }

newtype StreamID = StreamID {unSessionID :: BS.ByteString}
  deriving (Eq, Show)

-- data StreamValue = Numeric Double | Textual Integer | Boolean Bool

-- class Monad m => MonadStream m where
--   -- | A function for generating a random session ID.
--   -- This function should return a different value each time it's called.
--   generateStreamID :: m StreamID
--   getStreamPool :: m (Concurrent.MVar (Map.Map StreamID WS.Connection))
--   newNumeric :: Double -> m StreamID
--   default newNumeric :: Double -> m StreamID
--   newNumeric double = do
--     newStreamID <-
--     streamPool getStreamPool
--   newTextual :: Text.Text -> m StreamID
--   newBoolean :: Bool -> m StreamID
