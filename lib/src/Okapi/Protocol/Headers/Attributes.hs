{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Okapi.Protocol.Headers.Attributes (
    Attributes,
    attribute,
    attribute',
    flag,
    flag',
    raw,
    maxAge,
    domain,
    path,
    secure,
    httpOnly,
    parse,
    print,
) where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS8
import Data.Char (toLower)
import Data.Kind (Type)
import Data.List (find)
import Okapi.Codec (Codec (..), ParseErrorOf, StateOf)
import Okapi.Codec qualified as Codec
import Okapi.Data (Data (..), Iso (..))
import Okapi.Protocol.Headers.Cookies (Cookie, ParseError (..))
import Prelude hiding (print)

type Attributes :: Type -> Type
data Attributes a where
    Attr  :: ByteString -> Iso Cookie a -> Attributes a
    Attr' :: ByteString -> Iso Cookie a -> Attributes (Maybe a)
    Flag  :: ByteString -> Attributes ()
    Flag' :: ByteString -> Attributes Bool
    Raw   :: Attributes ByteString

type instance StateOf      Attributes = ByteString
type instance ParseErrorOf Attributes = ParseError

attribute :: ByteString -> Iso Cookie a -> Codec Attributes a a
attribute key vIso = Lift (Attr key vIso)

attribute' :: ByteString -> Iso Cookie a -> Codec Attributes (Maybe a) (Maybe a)
attribute' key vIso = Lift (Attr' key vIso)

flag :: ByteString -> Codec Attributes () ()
flag = Lift . Flag

flag' :: ByteString -> Codec Attributes Bool Bool
flag' = Lift . Flag'

raw :: Codec Attributes ByteString ByteString
raw = Lift Raw

maxAge :: Codec Attributes Int Int
maxAge = attribute "Max-Age" (iso @Cookie @Int)

domain :: Codec Attributes ByteString ByteString
domain = attribute "Domain" (iso @Cookie @ByteString)

path :: Codec Attributes ByteString ByteString
path = attribute "Path" (iso @Cookie @ByteString)

secure :: Codec Attributes () ()
secure = flag "Secure"

httpOnly :: Codec Attributes () ()
httpOnly = flag "HttpOnly"

parse :: Codec Attributes i o -> ByteString -> (Either ParseError o, ByteString)
parse = Codec.parser alg
  where
    alg :: forall a. Attributes a -> ByteString -> (Either ParseError a, ByteString)
    alg (Attr key vIso) s = case look key s of
        Just (Just v) -> case vIso.decode v of
            Left _  -> (Left ParseError, s)
            Right x -> (Right x, s)
        _ -> (Left ParseError, s)
    alg (Attr' key vIso) s = case look key s of
        Just (Just v) -> (Right (either (const Nothing) Just (vIso.decode v)), s)
        _             -> (Right Nothing, s)
    alg (Flag key) s = case look key s of
        Just _  -> (Right (), s)
        Nothing -> (Left ParseError, s)
    alg (Flag' key) s = (Right (maybe False (const True) (look key s)), s)
    alg Raw s = (Right s, s)

print :: Codec Attributes i o -> i -> ByteString
print = Codec.printer pr
  where
    pr :: forall a. Attributes a -> a -> ByteString
    pr (Attr key vIso)  v        = "; " <> key <> "=" <> vIso.encode v
    pr (Attr' key vIso) (Just v) = "; " <> key <> "=" <> vIso.encode v
    pr (Attr' _ _)      Nothing  = ""
    pr (Flag key)       ()       = "; " <> key
    pr (Flag' key)      True     = "; " <> key
    pr (Flag' _)        False    = ""
    pr Raw              bs       = bs

look :: ByteString -> ByteString -> Maybe (Maybe ByteString)
look key s = fmap snd (find ((== lc key) . fst) entries)
  where
    entries = [ breakEq (strip a) | a <- BS.split 59 s, not (BS.null (strip a)) ]
    breakEq a = case BS.elemIndex 61 a of
        Nothing -> (lc (strip a), Nothing)
        Just i  -> (lc (strip (BS.take i a)), Just (strip (BS.drop (i + 1) a)))

lc :: ByteString -> ByteString
lc = BS8.map toLower

strip :: ByteString -> ByteString
strip = BS.dropWhileEnd isSp . BS.dropWhile isSp
  where isSp w = w == 32 || w == 9
