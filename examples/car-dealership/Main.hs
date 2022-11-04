{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Main where

import Data.Text
import Okapi

import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Data.IORef
import Data.Map
import GHC.Generics
import Data.List.NonEmpty
import Database.SQLite.Simple
import Database.SQLite.Simple.FromField
import Database.SQLite.Simple.ToField
import Text.InterpolatedString.Perl6
import Control.Monad.Combinators
import Web.Internal.HttpApiData
import Web.Internal.FormUrlEncoded
import Data.ByteString
import System.Random


data Make = Toyota | Ford | Honda | Mercedes | BMW
  deriving (Eq, Show)

instance ToHttpApiData Make where
  toQueryParam = \case
    Toyota -> "toyota"
    Ford -> "ford"
    Honda -> "honda"
    Mercedes -> "mercedes"
    BMW -> "bmw"

instance FromHttpApiData Make where
  parseQueryParam = \case
    "toyota"   -> Right Toyota
    "ford"     -> Right Ford
    "honda"    -> Right Honda
    "mercedes" -> Right Mercedes
    "bmw"      -> Right BMW
    _          -> Left "Couldn't parse car make"

data Car = Car
  { carMake  :: Make
  , carYear  :: Int
  , carMiles :: Int
  , carPrice :: Float
  } deriving (Eq, Show, Generic, FromForm)

pattern HomeRoute = (GET, [])

pattern QueryCarsRoute = (GET, ["cars"])

pattern PostCarsRoute = (POST, ["cars"])

pattern PostSuccessRoute = (GET, ["cars", "post", "success"])

pattern PostFailureRoute = (GET, ["cars", "post", "failure"])

renderURL :: (Method, Path) -> Text
renderURL (_, p) = renderPath p

renderFormAttrs :: (Method, Path) -> Text
renderFormAttrs (m, p) = renderAction p <> " " <> renderMethod m
  where
    renderAction p = "action=\"" <> renderPath p <> "\""
    renderMethod = \case
      POST -> "method=\"" <> "post" <> "\""
      _    -> "method=\"" <> "get" <> "\"" -- ^ method="get" is the default method for forms

methodAndPathParser :: MonadHTTP m => m (Method, Path)
methodAndPathParser = do
  m <- method
  p <- path
  return (m, p)

methodAndPathDispatcher :: (MonadHTTP m, MonadIO m) => IORef [Car] -> (Method, Path) -> m Response
methodAndPathDispatcher database = \case
  HomeRoute -> do
    let html =
          [qq|
            <h1>Welcome to the online car dealership!</h1>
            <hr>
            <h2>Query Cars</h2>
            <form {renderFormAttrs QueryCarsRoute}>
              <label for="make">Car Make: </label>
              <select name="make" id="make" multiple>
                <option value={toQueryParam Toyota}>Toyota</option>
                <option value={toQueryParam Ford}>Ford</option>
                <option value={toQueryParam Honda}>Honda</option>
                <option value={toQueryParam Mercedes}>Mercedes</option>
                <option value={toQueryParam BMW}>BMW</option>
              </select>
              <br>
              <label for="year">Car Latest Year: </label>
              <input type="range" id="year" name="year" min="1985" step="1" max="2022" value="2022" oninput="this.nextElementSibling.value = this.value">
              <output>2022</output>
              <br>
              <label for="miles">Car Max Miles: </label>
              <input type="range" id="miles" name="miles" min="0" step="50000" max="500000" value="500000" oninput="this.nextElementSibling.value = this.value">
              <output>500000</output>
              <br>
              <label for="price">Car Max Price: </label>
              <input type="range" id="price" name="price" min="0" step="1000" max="200000" value="200000" oninput="this.nextElementSibling.value = this.value">
              <output>200000</output>
              <br>
              <input type="submit" value="Submit">
            </form>
            <hr>
            <h2>Put Your Car Up For Sale</h2>
            <form {renderFormAttrs PostCarsRoute}>
              <label for="carMake">Car Make: </label>
              <select name="carMake" id="carMake">
                <option value={toQueryParam Toyota}>Toyota</option>
                <option value={toQueryParam Ford}>Ford</option>
                <option value={toQueryParam Honda}>Honda</option>
                <option value={toQueryParam Mercedes}>Mercedes</option>
                <option value={toQueryParam BMW}>BMW</option>
              </select>
              <br>
              <label for="carYear">Car Year: </label>
              <select name="carYear" id="carYear">
                {Data.ByteString.concat $ Prelude.map makeYearOption [1985..2022]}
              </select>
              <br>
              <label for="carMiles">Car Miles: </label>
              <input type="range" id="carMiles" name="carMiles" min="0" step="50000" max="500000" value="200000" oninput="this.nextElementSibling.value = this.value">
              <output>200000</output>
              <br>
              <label for="carPrice">Car Price: </label>
              <input type="range" id="carPrice" name="carPrice" min="0" step="1000" max="200000" value="20000" oninput="this.nextElementSibling.value = this.value">
              <output>20000</output>
              <br>
              <input type="submit" value="Submit">
            </form>
          |]
    return $ setHTML html $ ok
  QueryCarsRoute -> do
    maybeMakes <- optional $ queryParamList @Make "make"
    latestYear <- queryParam @Int "year"
    maxMiles <- queryParam @Int "miles"
    maxPrice <- queryParam @Float "price"

    carsThatMatchQuery <- liftIO $ do
      let makes = case maybeMakes of
            Nothing -> []
            Just (m :| ms) -> m : ms
      availableCars <- readIORef database
      return $ filterCars makes maxMiles maxPrice availableCars

    let html =
          if Prelude.null carsThatMatchQuery
            then
              [qq|
                <h1>No results match your query.</h1>
                <a href="{renderURL HomeRoute}">Go back</a>
              |]
            else
              [qq|
                <table>
                  <tr>
                    <th>Make</th>
                    <th>Year</th>
                    <th>Miles</th>
                    <th>Price</th>
                  </tr>
                  {Data.ByteString.concat $ Prelude.map makeCarTableRow carsThatMatchQuery}
                </table>
                <a href="{renderURL HomeRoute}">Go back</a>
              |]

    return $ setHTML html $ ok
  PostCarsRoute -> do
    maybeCarForSale <- optional $ bodyURLEncoded @Car
    case maybeCarForSale of
      Nothing -> return $ redirect 302 $ renderURL PostFailureRoute 
      Just carForSale -> do
        liftIO $ modifyIORef database (carForSale :)
        return $ redirect 302 $ renderURL PostSuccessRoute
  PostSuccessRoute -> do
    let html =
          [qq|
            <h1>
              Your car is now up for sale!
            </h1>
            <a href="{renderURL HomeRoute}">Go back</a>
          |]
    return $ setHTML html $ ok
  PostFailureRoute -> do
    let html =
          [qq|
            <h1>
              We can't put your car up for sale.
              Make sure you entered valid data.
            </h1>
            <a href="{renderURL HomeRoute}">Go back</a>
          |]
    return $ setHTML html $ ok
  _ -> Okapi.next

main :: IO ()
main = do
  database <- newIORef []
  run id $ route methodAndPathParser $ methodAndPathDispatcher database

makeYearOption :: Int -> ByteString
makeYearOption year = [qq|<option value={toQueryParam year}>{toQueryParam year}</option>|]

makeCarTableRow :: Car -> ByteString
makeCarTableRow Car{..} =
  [qq|
    <tr>
      <td>{carMake}</td>
      <td>{show carYear}</td>
      <td>{show carMiles}</td>
      <td>${show carPrice}</td>
    </tr>
  |]

filterCars :: [Make] -> Int -> Float -> [Car] -> [Car]
filterCars makes maxMiles maxPrice cars =
  [ car
  | car <- cars
  , carMiles car <= maxMiles
  , carPrice car <= maxPrice
  , if Prelude.null makes then True else carMake car `Prelude.elem` makes
  ]
