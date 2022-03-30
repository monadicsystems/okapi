{-# LANGUAGE OverloadedStrings #-}

module Template where

import Data.Text
import Lucid

data Index = Index

instance ToHtml Index where
  toHtml _ =
    div_ [] $ do
      h1_ [] "Welcome to the front page of this calculator app! This was handled by servant."
      h1_ [] "Go to one of the links below to learn how to use the calculator apps:"
      a_ [href_ "servant"] "The servant calculator app"
      br_ []
      a_ [href_ "okapi"] "The okapi calculator app"
  toHtmlRaw = toHtml

data Greeting = Greeting Text

instance ToHtml Greeting where
  toHtml (Greeting framework) = do
    h1_ [] $ toHtml $ "Welcome to the " <> framework <> " calculator app! Thank you for choosing " <> framework <> "!"
    h1_ [] "I can do everything the other framework can do. Try me!"
    h1_ [] $ toHtml ("I can calculate stuff for you if you go to the URLs of the form:" :: Text)
    h1_ [] $ toHtml $ "{host}:{port}/" <> framework <> "/calc/{operation}/{x}/{y}"
    h1_ [] "OR"
    h1_ [] $ toHtml $ "{host}:{port}/" <> framework <> "/calc/{operation}?x={x}&y={y}"
    h1_ [] "{host} - the address of the place you're running me on"
    h1_ [] "{port} - whatever number you're using in the Warp.run function. It should be 8080 unless you changed it."
    h1_ [] "{operation} - the operation you want me to perform. It can be 'add', 'sub' or 'minus', 'mul', or 'div'. Remove the single quotes or it won't work. Please don't divide by 0!"
    h1_ [] "{x} - the first argument of the operation."
    h1_ [] "{y} - the second argument of the operation. If you use 'div', please make sure this argument isn't 0!"
    a_ [href_ $ framework <> "/calc/add/100/54"] $ h1_ [] "Try this one!"
  toHtmlRaw = toHtml

data AddResult = AddResult Int Int Int

instance ToHtml AddResult where
  toHtml (AddResult x y a) =
    h1_ [] $ toHtml $ showT x <> " + " <> showT y <> " = " <> showT a
  toHtmlRaw = toHtml

data SubResult = SubResult Int Int Int

instance ToHtml SubResult where
  toHtml (SubResult x y a) =
    h1_ [] $ toHtml $ showT x <> " - " <> showT y <> " = " <> showT a
  toHtmlRaw = toHtml

data MulResult = MulResult Int Int Int

instance ToHtml MulResult where
  toHtml (MulResult x y a) =
    h1_ [] $ toHtml $ showT x <> " * " <> showT y <> " = " <> showT a
  toHtmlRaw = toHtml

data DivResult
  = DivByZero
  | DivResult
      { dividend :: Int,
        divisor :: Int,
        answer :: Int,
        remainder :: Int
      }
  deriving (Eq, Show)

instance ToHtml DivResult where
  toHtml DivByZero =
    h1_ [] "Oh no you divided by 0! What have you done!?"
  toHtml (DivResult dividend divisor answer remainder) =
    h1_ [] $ toHtml $ showT dividend <> " / " <> showT divisor <> " = " <> showT answer <> " with a remainder of " <> showT remainder
  toHtmlRaw = toHtml

showT :: Show a => a -> Text
showT = pack . show
