{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

module TEA where

import qualified Control.Natural as Natural

newtype HTML = HTML String

data TEA env msg model = TEA
    { view :: model -> HTML
    , update :: msg -> model -> env model
    , trans :: env Natural.~> IO
    }

init :: TEA env msg model -> model -> IO HTML
init tea model = undefined
