module Main where

import qualified Control.Monad.Combinators as Combinators
import qualified Control.Monad.IO.Class as IO
import qualified Control.Monad.Reader as Reader
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Int as Int
import Data.Text as Text
import qualified Hasql.Connection as Hasql (settings)
import qualified Hasql.Pool as Hasql
import Lucid
import qualified Lucid
import qualified Lucid.Htmx as Htmx
import Main (UpdateContact)
import qualified Okapi
import qualified Rel8
import qualified Text.InterpolatedString.Perl6 as Perl6

newtype App a = App
  { unApp :: Reader.ReaderT AppEnv IO a
  }
  deriving newtype (Functor, Applicative, Monad, IO.MonadIO, Reader.MonadReader AppEnv)

execute :: AppEnv -> App a -> IO a
execute appEnv (App app) = Reader.runReaderT app appEnv

data AppEnv = AppEnv
  { getDBPool :: Hasql.Pool
  }

class Has field env where
  obtain :: env -> field

instance Has Hasql.Pool AppEnv where
  obtain = getDBPool

grab :: forall field env m. (Reader.MonadReader env m, Has field env) => m field
grab = Reader.asks $ obtain @field

main :: IO ()
main = do
  secret <- BS.readFile "secret.txt"
  let dbSettings = Hasql.settings "localhost" 5432 "realworld" secret "realworld"
  dbPool <- Hasql.acquire 100 (Just 3) dbSettings
  Okapi.run (execute $ AppEnv dbPool) contactsServer

pattern GetContacts :: (Okapi.Method, Okapi.Path)
pattern GetContacts = (Okapi.GET, ["contacts"])

pattern CreateContact :: (Okapi.Method, Okapi.Path)
pattern CreateContact = (Okapi.POST, ["contacts"])

pattern EditContact :: Int.Int64 -> (Okapi.Method, Okapi.Path)
pattern EditContact contactID = (Okapi.GET, ["contacts", "edit", Okapi.PathParam contactID])

pattern UpdateContact :: Int.Int64 -> (Okapi.Method, Okapi.Path)
pattern UpdateContact contactID = (Okapi.PUT, ["contacts", Okapi.PathParam contactID])

pattern DeleteContact :: Int.Int64 -> (Okapi.Method, Okapi.Path)
pattern DeleteContact contactID = (Okapi.DELETE, ["contacts", Okapi.PathParam contactID])

toURL :: (Okapi.Method, Okapi.Path) -> Text.Text
toURL (_, path) = Okapi.renderPath path

contactsServer :: (Okapi.MonadServer m, IO.MonadIO m) => m ()
contactsServer = wrapPartialIfNotHtmxRequest $ Okapi.route routeParser \case
  GetContacts -> do
    let myContacts =
          [ Contact 1 "Rashad" "hellorashad@gmail.com",
            Contact 2 "Derek" "bod@fisher.com",
            Contact 3 "Brian" "brian@goodie.com"
          ]
    writeLucid myContacts
  CreateContact -> undefined
  EditContact contactID -> undefined
  UpdateContact contactID -> undefined
  DeleteContact contactID -> undefined
  _ -> Okapi.next

routeParser :: Okapi.MonadServer m => m (Okapi.Method, Okapi.Path)
routeParser = do
  parsedMethod <- Okapi.method
  parsedPath <- Okapi.path
  pure (parsedMethod, parsedPath)

wrapPartialIfNotHtmxRequest :: Okapi.MonadServer m => Okapi.Middleware m
wrapPartialIfNotHtmxRequest server = do
  hxRequestHeaderValue <- Combinators.optional $ Okapi.header "HX-Request"
  case hxRequestHeaderValue of
    Just "true" -> server
    _ -> do
      Okapi.write top
      server
      Okapi.write bottom
  where
    top :: LBS.ByteString
    top =
      [Perl6.q|
    <!DOCTYPE html>
    <html>

    <head>
        <meta charset="utf-8">
        <meta name="viewport" content="width=device-width, initial-scale=1.0">
        <title>Mercury Meetup Contacts</title>
        <script src="https://unpkg.com/htmx.org@1.8.0"></script>
        <script src="https://cdn.tailwindcss.com?plugins=forms"></script>
    </head>

    <body class="flex h-screen justify-center items-center">
      <main class="w-full px-10">
        <h1 class="text-xl font-semibold mb-2">Haskell Talks @ Mercury Contact List</h1>
    |]

    bottom :: LBS.ByteString
    bottom =
      [Perl6.q|
      </main>
    </body>

    </html>
    |]

data ContactRow f = ContactRow
  { key :: Rel8.Column f Int.Int64,
    name :: Rel8.Column f Text.Text,
    email :: Rel8.Column f Text.Text
  }

contactTableSchema :: Rel8.TableSchema (ContactRow Rel8.Name)
contactTableSchema =
  Rel8.TableSchema
    { name = "contacts",
      schema = Nothing,
      columns =
        ContactRow
          { key = "pk_contacts",
            name = "name",
            email = "email"
          }
    }

data Contact = Contact
  { key :: Int.Int64,
    name :: Text.Text,
    email :: Text.Text
  }

{-
<tr>
      <td>${contact.name}</td>
      <td>${contact.email}</td>
      <td>
        <button class="btn btn-danger"
                hx-get="/contact/${contact.id}/edit"
                hx-trigger="edit"
                _="on click
                     if .editing is not empty
                       Swal.fire({title: 'Already Editing',
                                  showCancelButton: true,
                                  confirmButtonText: 'Yep, Edit This Row!',
                                  text:'Hey!  You are already editing a row!  Do you want to cancel that edit and continue?'})
                       if the result's isConfirmed is false
                         halt
                       end
                       send cancel to .editing
                     end
                     trigger edit">
          Edit
        </button>
      </td>
    </tr>
-}

contactToTableRow :: Contact -> Lucid.Html ()
contactToTableRow (Contact key name email) =
  tr_ [] do
    td_ [class_ "whitespace-nowrap py-4 pl-4 pr-3 text-md font-medium text-gray-900 sm:pl-6"] $ Lucid.toHtml name
    td_ [class_ "whitespace-nowrap px-3 py-4 text-md text-gray-500"] $ Lucid.toHtml email
    td_ [class_ "whitespace-nowrap flex px-3 py-4 text-md text-gray-500"] do
      button_
        [ class_ "inline-flex items-center rounded-md border border-gray-300 bg-white px-3 py-2 text-md font-medium leading-4 text-gray-700 shadow-sm hover:bg-gray-50",
          Htmx.hxGet_ $ toURL $ EditContact key
        ]
        "Edit"
      button_
        [ class_ "ml-2 inline-flex items-center rounded-md border border-gray-300 bg-red-500 px-3 py-2 text-md font-medium leading-4 text-gray-700 shadow-sm hover:bg-red-600",
          Htmx.hxDelete_ $ toURL $ DeleteContact key,
          Htmx.hxConfirm_ $ "Are you sure want to delete the contact information for " <> name <> "?"
        ]
        "Delete"

contactToEditForm :: Contact -> Lucid.Html ()
contactToEditForm (Contact key name email) =
  tr_ [] do
    td_ [class_ "whitespace-nowrap py-4 pl-4 pr-3 text-md font-medium text-gray-900 sm:pl-6"] $
      input_
        [ type_ "text",
          name_ "name",
          value_ name,
          class_ "block w-full rounded-md border-gray-500 shadow-sm focus:border-slate-500 focus:ring-slate-500 sm:text-sm"
        ]
    td_ [class_ "whitespace-nowrap px-3 py-4 text-md text-gray-500"] $
      input_
        [ type_ "email",
          name_ "email",
          value_ email,
          class_ "block w-full rounded-md border-gray-500 shadow-sm focus:border-slate-500 focus:ring-slate-500 sm:text-sm"
        ]
    td_ [class_ "whitespace-nowrap flex px-3 py-4 text-md text-gray-500"] do
      button_
        [ class_ "inline-flex items-center rounded-md border border-gray-300 bg-green-300 px-3 py-2 text-md font-medium leading-4 text-gray-700 shadow-sm hover:bg-green-400",
          Htmx.hxInclude_ "closest tr",
          Htmx.hxPut_ $ toURL $ UpdateContact key
        ]
        "Save"
      button_
        [ class_ "inline-flex items-center rounded-md border border-gray-300 bg-green-300 px-3 py-2 text-md font-medium leading-4 text-gray-700 shadow-sm hover:bg-green-400",
          Htmx.hxGet_ $ toURL $ GetContact key
        ]
        "Cancel"

{-
  <div class="mt-8 flex flex-col">
    <div class="-my-2 -mx-4 overflow-x-auto sm:-mx-6 lg:-mx-8">
      <div class="inline-block min-w-full py-2 align-middle md:px-6 lg:px-8">
        <div class="overflow-hidden shadow ring-1 ring-black ring-opacity-5 md:rounded-lg">
          <table class="min-w-full divide-y divide-gray-300">
-}

contactsToTable :: [Contact] -> Lucid.Html ()
contactsToTable =
  div_ [class_ "mt-8 flex flex-col"] do
    div_ [class_ "-my-2 -mx-4 overflow-x-auto sm:-mx-6 lg:-mx-8"] do
      div_ [class_ "inline-block min-w-full py-2 align-middle md:px-6 lg:px-8"] do
        div_ [class_ "overflow-hidden shadow ring-1 ring-black ring-opacity-5 md:rounded-lg"] do
          table_ [class_ "min-w-full divide-y divide-gray-300"] do
            thead_ [class_ "bg-gray-50"] do
              tr_ [] do
                th_ [class_ "py-3.5 pl-4 pr-3 text-left text-md font-semibold text-gray-900 sm:pl-6"] "Name"
                th_ [class_ "px-3 py-3.5 text-left text-md font-semibold text-gray-900"] "Email"
                th_ [class_ "px-3 py-3.5 text-left text-md font-semibold text-gray-900"] "Actions"
            tbody_ [Htmx.hxTarget_ "closest tr", Htmx.hxSwap_ "outerHTML", class_ "divide-y divide-gray-200 bg-white"] do
              mapM_ contactToTableRow contacts
              tr_ [] do
                td_ [class_ "whitespace-nowrap py-4 pl-4 pr-3 text-md font-medium text-gray-900 sm:pl-6"] $
                  input_
                    [ type_ "text",
                      name_ "name",
                      value_ "",
                      class_ "block w-full rounded-md border-gray-500 shadow-sm focus:border-slate-500 focus:ring-slate-500 sm:text-sm"
                    ]
                td_ [class_ "whitespace-nowrap px-3 py-4 text-md text-gray-500"] $
                  input_
                    [ type_ "email",
                      name_ "email",
                      value_ "",
                      class_ "block w-full rounded-md border-gray-500 shadow-sm focus:border-slate-500 focus:ring-slate-500 sm:text-sm"
                    ]
                td_ [class_ "whitespace-nowrap flex px-3 py-4 text-md text-gray-500"] do
                  button_
                    [ class_ "inline-flex items-center rounded-md border border-gray-300 bg-green-300 px-3 py-2 text-md font-medium leading-4 text-gray-700 shadow-sm hover:bg-green-400",
                      Htmx.hxInclude_ "closest tr",
                      Htmx.hxPost_ $ toURL CreateContact
                    ]
                    "Add"

toText :: Show a => a -> Text.Text
toText = Text.pack . show

blankHtml :: Monad m => HtmlT m ()
blankHtml = ""

{-
<table class="table delete-row-example">
  <thead>
    <tr>
      <th>Name</th>
      <th>Email</th>
      <th></th>
    </tr>
  </thead>
  <tbody hx-target="closest tr" hx-swap="outerHTML">
    ...
  </tbody>
</table>
-}

data ContactForm = ContactForm
  { name :: Text.Text,
    email :: Text.Text
  }

instance Okapi.Writeable (Lucid.Html ()) where
  toLBS :: Lucid.Html () -> LBS.ByteString
  toLBS = Lucid.renderBS

writeLucid :: (Okapi.MonadServer m, Lucid.ToHtml a) => a -> m ()
writeLucid htmlable = do
  Okapi.setHeader ("Content-Type", "text/html;charset=utf-8")
  Okapi.write $ toHtmlPure htmlable
  where
    toHtmlPure :: Lucid.ToHtml a => a -> Lucid.Html ()
    toHtmlPure = Lucid.toHtml
