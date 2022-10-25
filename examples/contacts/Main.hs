module Main where

import qualified Control.Monad.Combinators as Combinators
import qualified Control.Monad.IO.Class as IO
import qualified Control.Monad.Reader as Reader
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Int as Int
import qualified Data.Maybe as Maybe
import Data.Text as Text
import qualified GHC.Generics as Generics
import qualified Hasql.Connection as Hasql (settings)
import qualified Hasql.Pool as Hasql
import qualified Hasql.Statement as Hasql
import qualified Hasql.Transaction as Hasql
import qualified Hasql.Transaction.Sessions as Hasql
import Lucid
import qualified Lucid
import qualified Lucid.Htmx as Htmx
import qualified Okapi
import Rel8 ((==.))
import qualified Rel8
import qualified Text.InterpolatedString.Perl6 as Perl6
import qualified Web.FormUrlEncoded as Web

main :: IO ()
main = do
  secret <- BS.readFile "secret.txt"
  let dbSettings = Hasql.settings "localhost" 5432 "realworld" secret "realworld"
  dbPool <- Hasql.acquire 100 (Just 2) dbSettings
  Okapi.run (execute $ AppEnv dbPool) contactsServer

-- Core data types

data ContactForm = ContactForm
  { contactFormName :: Text.Text,
    contactFormEmail :: Text.Text
  }
  deriving (Generics.Generic, Web.FromForm)

data Contact = Contact
  { contactID :: Int.Int64,
    contactName :: Text.Text,
    contactEmail :: Text.Text
  }

-- Our App Monad: Has access to an AppEnv and can perform IO actions

newtype App a = App
  { unApp :: Reader.ReaderT AppEnv IO a
  }
  deriving newtype (Functor, Applicative, Monad, IO.MonadIO, Reader.MonadReader AppEnv)

execute :: AppEnv -> App a -> IO a
execute appEnv (App app) = Reader.runReaderT app appEnv

newtype AppEnv = AppEnv
  { getDBPool :: Hasql.Pool
  }

class Has field env where
  obtain :: env -> field

instance Has Hasql.Pool AppEnv where
  obtain = getDBPool

grab :: forall field env m. (Reader.MonadReader env m, Has field env) => m field
grab = Reader.asks $ obtain @field

-- The server

contactsServer :: (Okapi.ServerM m, Reader.MonadReader AppEnv m, IO.MonadIO m) => m ()
contactsServer = wrapPartialIfNotHtmxRequest $ Okapi.route routeParser \case
  GetContacts -> do
    allContacts <- getAllContacts
    writeLucid $ contactsToTable allContacts
  CreateContact -> do
    contactForm <- Okapi.bodyURLEncoded @ContactForm
    newContact <- insertContact contactForm
    writeLucid do
      contactToRow newContact
      addContactRow
  GetContact contactID -> do
    storedContact <- getContactByID contactID
    writeLucid $ contactToRow storedContact
  EditContact contactID -> do
    storedContact <- getContactByID contactID
    writeLucid $ contactToEditContactRow storedContact
  UpdateContact contactID -> do
    contactForm <- Okapi.bodyURLEncoded @ContactForm
    updatedContact <- updateContact contactID contactForm
    writeLucid $ contactToRow updatedContact
  DeleteContact contactID -> do
    deleteContact contactID
    writeLucid ""
  _ -> Okapi.next

-- Our route patterns for pattern matching on and constructing routes

pattern GetContacts :: (Okapi.Method, Okapi.Path)
pattern GetContacts = (Okapi.GET, ["contacts"])

pattern CreateContact :: (Okapi.Method, Okapi.Path)
pattern CreateContact = (Okapi.POST, ["contacts"])

pattern GetContact :: Int.Int64 -> (Okapi.Method, Okapi.Path)
pattern GetContact contactID = (Okapi.GET, ["contacts", Okapi.PathParam contactID])

pattern EditContact :: Int.Int64 -> (Okapi.Method, Okapi.Path)
pattern EditContact contactID = (Okapi.GET, ["contacts", "edit", Okapi.PathParam contactID])

pattern UpdateContact :: Int.Int64 -> (Okapi.Method, Okapi.Path)
pattern UpdateContact contactID = (Okapi.PUT, ["contacts", Okapi.PathParam contactID])

pattern DeleteContact :: Int.Int64 -> (Okapi.Method, Okapi.Path)
pattern DeleteContact contactID = (Okapi.DELETE, ["contacts", Okapi.PathParam contactID])

-- For converting our route patterns into URLs
-- TODO: Add function for converting route patterns to Htmx attributes
toURL :: (Okapi.Method, Okapi.Path) -> Text.Text
toURL (_, path) = Okapi.renderPath path

toHtmxAttribute :: (Okapi.Method, Okapi.Path) -> Lucid.Attributes
toHtmxAttribute (method, path) = case method of
  Okapi.GET -> Htmx.hxGet_ url
  Okapi.POST -> Htmx.hxPost_ url
  Okapi.PUT -> Htmx.hxPut_ url
  Okapi.DELETE -> Htmx.hxDelete_ url
  _ -> class_ ""
  where
    url = Okapi.renderPath path

-- View generating functions

contactsToTable :: [Contact] -> Lucid.Html ()
contactsToTable contacts =
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
              mapM_ contactToRow contacts
              addContactRow

contactToRow :: Contact -> Lucid.Html ()
contactToRow (Contact id' name email) =
  tr_ [] do
    td_ [class_ "whitespace-nowrap py-4 pl-4 pr-3 text-md font-medium text-gray-900 sm:pl-6"] $ Lucid.toHtml name
    td_ [class_ "whitespace-nowrap px-3 py-4 text-md text-gray-500"] $ Lucid.toHtml email
    td_ [class_ "whitespace-nowrap flex px-3 py-4 text-md text-gray-500"] do
      button_
        [ class_ "inline-flex items-center rounded-md border border-gray-300 bg-indigo-400 px-3 py-2 text-md font-medium leading-4 text-gray-700 shadow-sm hover:bg-indigo-500",
          Htmx.hxGet_ $ toURL $ EditContact id'
        ]
        "Edit"
      button_
        [ class_ "ml-2 inline-flex items-center rounded-md border border-gray-300 bg-red-400 px-3 py-2 text-md font-medium leading-4 text-gray-700 shadow-sm hover:bg-red-500",
          toHtmxAttribute $ DeleteContact id',
          Htmx.hxConfirm_ $ "Are you sure want to delete the contact information for " <> name <> "?"
        ]
        "Delete"

addContactRow :: Lucid.Html ()
addContactRow =
  tr_ [] do
    td_ [class_ "whitespace-nowrap py-4 pl-4 pr-3 text-md font-medium text-gray-900 sm:pl-6"] $
      input_
        [ type_ "text",
          name_ "contactFormName",
          value_ "",
          class_ "block w-full rounded-md border-gray-500 shadow-sm focus:border-slate-500 focus:ring-slate-500 sm:text-sm"
        ]
    td_ [class_ "whitespace-nowrap px-3 py-4 text-md text-gray-500"] $
      input_
        [ type_ "email",
          name_ "contactFormEmail",
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

contactToEditContactRow :: Contact -> Lucid.Html ()
contactToEditContactRow (Contact id' name email) =
  tr_ [] do
    td_ [class_ "whitespace-nowrap py-4 pl-4 pr-3 text-md font-medium text-gray-900 sm:pl-6"] $
      input_
        [ type_ "text",
          name_ "contactFormName",
          value_ name,
          class_ "block w-full rounded-md border-gray-500 shadow-sm focus:border-slate-500 focus:ring-slate-500 sm:text-sm"
        ]
    td_ [class_ "whitespace-nowrap px-3 py-4 text-md text-gray-500"] $
      input_
        [ type_ "email",
          name_ "contactFormEmail",
          value_ email,
          class_ "block w-full rounded-md border-gray-500 shadow-sm focus:border-slate-500 focus:ring-slate-500 sm:text-sm"
        ]
    td_ [class_ "whitespace-nowrap flex px-3 py-4 text-md text-gray-500"] do
      button_
        [ class_ "inline-flex items-center rounded-md border border-gray-300 bg-green-300 px-3 py-2 text-md font-medium leading-4 text-gray-700 shadow-sm hover:bg-green-400",
          Htmx.hxInclude_ "closest tr",
          Htmx.hxPut_ $ toURL $ UpdateContact id'
        ]
        "Save"
      button_
        [ class_ "ml-2 inline-flex items-center rounded-md border border-gray-300 bg-red-400 px-3 py-2 text-md font-medium leading-4 text-gray-700 shadow-sm hover:bg-red-500",
          Htmx.hxGet_ $ toURL $ GetContact id'
        ]
        "Cancel"

-- Database interactions

data ContactRow f = ContactRow
  { contactRowID :: Rel8.Column f Int.Int64,
    contactRowName :: Rel8.Column f Text.Text,
    contactRowEmail :: Rel8.Column f Text.Text
  }
  deriving stock (Generics.Generic)
  deriving anyclass (Rel8.Rel8able)

deriving stock instance f ~ Rel8.Result => Show (ContactRow f)

contactsTableSchema :: Rel8.TableSchema (ContactRow Rel8.Name)
contactsTableSchema =
  Rel8.TableSchema
    { name = "contacts",
      schema = Nothing,
      columns =
        ContactRow
          { contactRowID = "pk_contacts",
            contactRowName = "name",
            contactRowEmail = "email"
          }
    }

getAllContacts :: (Okapi.ServerM m, Reader.MonadReader AppEnv m, IO.MonadIO m) => m [Contact]
getAllContacts = do
  allContactRows <- runStatement $ Rel8.select queryAllContacts
  pure $ contactRowToContact <$> allContactRows

getContactByID :: (Okapi.ServerM m, Reader.MonadReader AppEnv m, IO.MonadIO m) => Int.Int64 -> m Contact
getContactByID contactID = do
  maybeContactRow <- runStatementOne $ Rel8.select $ queryContactByID contactID
  case maybeContactRow of
    Nothing -> Okapi.throw Okapi.internalServerError
    Just contactRow -> pure $ contactRowToContact contactRow

insertContact :: (Okapi.ServerM m, Reader.MonadReader AppEnv m, IO.MonadIO m) => ContactForm -> m Contact
insertContact contactForm = do
  maybeNewContactID <- runStatementOne $ Rel8.insert $ contactInsert contactForm
  case maybeNewContactID of
    Nothing -> Okapi.throw Okapi.internalServerError
    Just newContactID -> getContactByID newContactID

updateContact :: (Okapi.ServerM m, Reader.MonadReader AppEnv m, IO.MonadIO m) => Int.Int64 -> ContactForm -> m Contact
updateContact contactID contactForm = do
  rowsAffected <- runStatement $ Rel8.update $ contactUpdate contactID contactForm
  if rowsAffected == 1
    then getContactByID contactID
    else Okapi.throw Okapi.internalServerError

deleteContact :: (Okapi.ServerM m, Reader.MonadReader AppEnv m, IO.MonadIO m) => Int.Int64 -> m ()
deleteContact contactID = do
  rowsAffected <- runStatement $ Rel8.delete $ contactDelete contactID
  if rowsAffected == 1
    then pure ()
    else Okapi.throw Okapi.internalServerError

queryAllContacts :: Rel8.Query (ContactRow Rel8.Expr)
queryAllContacts = Rel8.orderBy Rel8.ascTable $ Rel8.each contactsTableSchema

queryContactByID :: Int.Int64 -> Rel8.Query (ContactRow Rel8.Expr)
queryContactByID contactID = do
  contactRow <- Rel8.each contactsTableSchema
  Rel8.where_ $ contactRowID contactRow ==. Rel8.lit contactID
  return contactRow

contactInsert :: ContactForm -> Rel8.Insert [Int.Int64]
contactInsert contactForm =
  Rel8.Insert
    { into = contactsTableSchema,
      rows =
        Rel8.values
          [ ContactRow
              { contactRowID = Rel8.unsafeCastExpr $ Rel8.nextval "contacts_pk_contacts_seq", -- TODO: Check if this is correct
                contactRowName = Rel8.lit $ contactFormName contactForm,
                contactRowEmail = Rel8.lit $ contactFormEmail contactForm
              }
          ],
      onConflict = Rel8.DoNothing,
      returning = Rel8.Projection contactRowID
    }

contactUpdate :: Int.Int64 -> ContactForm -> Rel8.Update Int.Int64
contactUpdate contactID contactForm =
  Rel8.Update
    { target = contactsTableSchema,
      updateWhere = \from o -> contactRowID o ==. Rel8.lit contactID,
      from = pure (),
      set = \from row ->
        row
          { contactRowName = Rel8.lit $ contactFormName contactForm,
            contactRowEmail = Rel8.lit $ contactFormEmail contactForm
          },
      returning = Rel8.NumberOfRowsAffected
    }

contactDelete :: Int.Int64 -> Rel8.Delete Int.Int64
contactDelete contactID =
  Rel8.Delete
    { from = contactsTableSchema,
      using = pure (),
      deleteWhere = \using row -> contactRowID row ==. Rel8.lit contactID,
      returning = Rel8.NumberOfRowsAffected
    }

runStatementOne :: (IO.MonadIO m, Okapi.ServerM m, Reader.MonadReader AppEnv m) => Hasql.Statement () [a] -> m (Maybe a)
runStatementOne statement = do
  rows <- runStatement statement
  pure $ Maybe.listToMaybe rows

runStatement :: (IO.MonadIO m, Okapi.ServerM m, Reader.MonadReader AppEnv m) => Hasql.Statement () o -> m o
runStatement statement = do
  let transaction = Hasql.statement () statement
      session = Hasql.transaction Hasql.Serializable Hasql.Write transaction
  dbPool <- grab @Hasql.Pool
  dbResult <- IO.liftIO $ Hasql.use dbPool session
  case dbResult of
    Left usageError -> Okapi.throw (Okapi.internalServerError {Okapi.responseBody = Okapi.ResponseBodyRaw [Perl6.qq| {usageError} |]})
    Right success -> pure success

-- Helpers

toText :: Show a => a -> Text.Text
toText = Text.pack . show

emptyHtml :: Monad m => HtmlT m ()
emptyHtml = ""

instance Okapi.Writeable (Lucid.Html ()) where
  toLBS :: Lucid.Html () -> LBS.ByteString
  toLBS = Lucid.renderBS

writeLucid :: Okapi.ServerM m => Lucid.Html () -> m ()
writeLucid html = do
  Okapi.setHeader ("Content-Type", "text/html;charset=utf-8")
  Okapi.write $ Lucid.renderBS html

routeParser :: Okapi.ServerM m => m (Okapi.Method, Okapi.Path)
routeParser = do
  parsedMethod <- Okapi.method
  parsedPath <- Okapi.path
  pure (parsedMethod, parsedPath)

wrapPartialIfNotHtmxRequest :: Okapi.ServerM m => Okapi.Middleware m
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
        <title>My Contact Book</title>
        <script src="https://unpkg.com/htmx.org@1.8.0"></script>
        <script src="https://cdn.tailwindcss.com?plugins=forms"></script>
    </head>

    <body class="flex justify-center items-center my-20">
      <main class="w-full px-10">
        <h1 class="text-xl font-semibold mb-2">My Contact Book</h1>
    |]

    bottom :: LBS.ByteString
    bottom =
      [Perl6.q|
      </main>
    </body>

    </html>
    |]

contactRowToContact :: ContactRow Rel8.Result -> Contact
contactRowToContact row =
  Contact
    { contactID = contactRowID row,
      contactName = contactRowName row,
      contactEmail = contactRowEmail row
    }
