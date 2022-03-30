{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module ServantAPI where

import Control.Applicative ((<|>))
import Data.Aeson
import GHC.Generics
import GHC.TypeLits
import Lucid
import Network.Wai.Handler.Warp
import OkapiAPI (okapiApplication)
import Servant hiding (QueryParam)
import Servant.API.Raw
import Servant.HTML.Lucid
import Servant.Server
import Template

apiHandler :: Server (ServantAPI :<|> Raw)
apiHandler = servantHandler :<|> (Tagged okapiApplication :: Tagged Handler Application)

apiProxy :: Proxy (ServantAPI :<|> Raw)
apiProxy = Proxy

type ServantAPI =
  Get '[HTML] Index
    :<|> ("servant" :> (Get '[HTML] Greeting :<|> CalcAPI))

servantHandler :: Server ServantAPI
servantHandler = pure Index :<|> (pure (Greeting "servant") :<|> calcHandler)

type CalcAPI =
  "calc"
    :> (AddOp :<|> SubOp :<|> MulOp :<|> DivOp)

calcHandler =
  (addOp :<|> addOp)
    :<|> ((subOp :<|> subOp) :<|> (subOp :<|> subOp))
    :<|> (mulOp :<|> mulOp)
    :<|> (divOp :<|> divOp)

type QueryParam = QueryParam' '[Required]

type AddOp =
  "add"
    :> ( (Capture "x" Int :> Capture "y" Int :> Get '[HTML] AddResult)
           :<|> (QueryParam "x" Int :> QueryParam "y" Int :> Get '[HTML] AddResult)
       )

addOp :: Int -> Int -> Handler AddResult
addOp x y = pure $ AddResult x y $ x + y

type SubOp =
  ( "sub"
      :> ( (Capture "x" Int :> Capture "y" Int :> Get '[HTML] SubResult)
             :<|> (QueryParam "x" Int :> QueryParam "y" Int :> Get '[HTML] SubResult)
         )
  )
    :<|> ( "minus"
             :> ( (Capture "x" Int :> Capture "y" Int :> Get '[HTML] SubResult)
                    :<|> (QueryParam "x" Int :> QueryParam "y" Int :> Get '[HTML] SubResult)
                )
         )

subOp :: Int -> Int -> Handler SubResult
subOp x y = pure $ SubResult x y $ x - y

type MulOp =
  "mul"
    :> ( (Capture "x" Int :> Capture "y" Int :> Get '[HTML] MulResult)
           :<|> (QueryParam "x" Int :> QueryParam "y" Int :> Get '[HTML] MulResult)
       )

mulOp :: Int -> Int -> Handler MulResult
mulOp x y = pure $ MulResult x y $ x * y

type DivOp =
  "div"
    :> ( (Capture "x" Int :> Capture "y" Int :> Get '[HTML] DivResult)
           :<|> (QueryParam "x" Int :> QueryParam "y" Int :> Get '[HTML] DivResult)
       )

divOp :: Int -> Int -> Handler DivResult
divOp x y =
  pure $
    if y == 0
      then DivByZero
      else
        DivResult
          { dividend = x,
            divisor = y,
            answer = x `div` y,
            remainder = x `mod` y
          }
