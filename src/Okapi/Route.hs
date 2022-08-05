{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

--- !!!!!!TODO: Change Name of Okapi to HTTParser??? :TODO!!!!!!

module Okapi.Route where

import Control.Applicative
import Control.Applicative.Combinators
import Control.Monad (forM)
import Control.Monad.IO.Class
import Data.Attoparsec.Text
import Data.Char (isAlpha, isAlphaNum, isUpper)
import Data.List.NonEmpty
import Data.Maybe (catMaybes, mapMaybe)
import Data.String (IsString)
import Data.Text
import GHC.ExecutionStack (Location (functionName))
import GHC.Unicode (isAscii)
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Okapi.Internal.Functions.Route
import System.Random

route :: QuasiQuoter
route =
  QuasiQuoter
    { quoteExp = genRouteExp . pack,
      quotePat = undefined,
      quoteType = undefined,
      quoteDec = undefined
    }
  where
    genRouteExp :: Text -> Q Exp
    genRouteExp txt = do
      let parserResult = parseOnly routeParser txt
      case parserResult of
        Left _ -> routePartsToExp []
        Right routeParts -> routePartsToExp routeParts

{-
newtype URL = URL Text

data URLParam = forall a. ToHttpApiData a => URLParam a

-- Maybe should just generate parser? But I thought the point was to get info for URL?
-- Maybe instead of generating URL have URL builder helpers?
data Route input = Route
  { parser :: Okapi Response
  , url    :: input -> URL
  }

sequencing routes
route1 <> route2 = Route
  { parser = do
      route1
      route2
  , url =
  }

data RouteParams = forall a. RouteParams (Map Text a)

-- (%%) :: (a -> URL) -> (b -> URL) -> (a -> b -> URL)
-- (%%) urlProducer urlProducer' = \a -> \b -> urlProducer a <> urlProducer' b

-- Example Usage

newtype BookID = BookID Int deriving (FromHttpApiData)

newtype Author = Author Text deriving (FromHttpApiData)

data URL = Empty | PathPiece Text | QueryPiece Text Text | Path URL URL

TODO: What TO DO with partial URLs???????????

cons :: URL -> URL -> URL
cons Empty Empty            = Empty
cons Empty pp@(PathPiece _) = Path Empty pp
cons Empty p@(Path _ _)     = p
cons pp1@(Path _ _)

-- Expand on type safe URL building. Will need it either way

data Genre = Fantasy | SciFi | NonFiction | Romance
-}

{-
-- booksRoute :: ? -- Type isn't known until code is generated at compile time
booksRoute = [Okapi.route|GET /books /{Maybe BookID} ?author{Maybe Author} ?genre{Maybe Genre}|]

booksURL :: URL
booksURL = booksRoute.url (Nothing, Just "Mark Twain", Nothing)

booksRouteNamed = [Okapi.genRoute|GET /books /:bookID ?author:authorName ?genre:|]

newProduct :: Okapi Response
newProduct = do
  validProductID <- [Okapi.genParser|POST /products/{ProductID|isValid}|]
  ...

books :: Okapi Response
books = booksRoute.parser >>= booksHandler
  where
    booksHandler :: (Maybe BookID, Maybe Author, Maybe Genre) -> Okapi Response
    booksHandler (maybeBookID, maybeAuthor, maybeGenre) = do
      ...

isModern :: Date -> Bool

childActors :: [Actor] -> [Actor]

bornInIndiana :: [Actor] -> [Actor]

-- Could be gen parser
-- parser generator for headers?
moviesRoute = [Okapi.route|
    GET
    HEAD
    /movies
    /{Date|isModern}
    ?director{Director}
    ?actors{[Actor]->childActors->bornInIndiana|notEmpty}
    ?female
|]

"GET HEAD /movies /{Date|isModern} ?director{Director} ?actors{[Actor]->childActors->bornInIndiana|notEmpty} ?female{Gender}"

THE ABOVE MAY BE PARTIAL ROUTES WITHOUT EVERY PIECE OF A URL

---------------------
-- Multiple Routes --
---------------------

[Okapi.genRoute|
  CHOOSE
    GET /                                        >> getHome
    GET /movies             >> authenticate      >> getMoviesHandler
    PUT /movies /{MovieID} >>= authenticateData >>= putMovieHandler
    GET /books              >> authenticate      >> getBooksHandler
    PUT /books /{BookID}   >>= authenticateData >>= putBookHandle
    GET /anotherPath >> anotherHandler

    someOtherRoute = ...
|]

authenticate :: Okapi ()

authenticateData :: forall a. a -> Okapi a

getMoviesHandler :: Okapi Response
getBooksHandler  :: Okapi Response
putMovieHandler  :: MovieID -> Okapi Response
putBookHandler   :: BookID  -> Okapi Response
anotherHandler   :: Okapi Response

-- Generates:
megaApp :: Okapi Response
megaApp = app <|> anotherApp

app :: Okapi Response -- top level parser
app = getMovieRoute.parser <|> putMovieRoute.parser <|> ...

anotherApp :: Okapi Response
anotherApp = anotherRoute.parser

getMoviesRoute
putMovieRoute
getBooksRoute
putBookRoute
anotherRoute

TODO: generate Haddock documentation too!
https://gitlab.haskell.org/ghc/ghc/-/commit/8a59f49ae2204dbf58ef50ea8c0a50ee2c7aa64a
-}
{-
-- genRoute creates a full route. Must have at least a method and return a response handler.
genRoute :: Q Exp
genRoute = undefined

-- Can use any part of the routing syntax to create a parser, but no URL is created just a parser.
genParser :: Q Exp
genParser = undefined

-- Generate entire top level parser. Basically multiple routes
genApp :: Q [Dec]
genApp = undefined

genOptimalApp :: Q [Dec]
genOptimalApp = undefined
-}
