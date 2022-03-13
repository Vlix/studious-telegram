{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
module Main where

import Data.Semigroup
import Data.Text (Text)
import GHC.Generics (Generic)
import           Servant hiding (NotSecure)
import           Servant.API.Generic
import           Servant.Server
import           Network.Wai.Handler.Warp   (defaultSettings, runSettings,
                                             setLogger, setPort)

import qualified Data.Text as T

type MainAPI = NamedRoutes API

data API mode = API
    { static1 :: mode :- "static" :> "1" :> Get '[JSON] Text
    , static2 :: mode :- "static" :> "2" :> Get '[JSON] Text
    , static3 :: mode :- "static" :> "3" :> Get '[JSON] Text
    , static4 :: mode :- "static" :> "4" :> Get '[JSON] Text
    , static5 :: mode :- "static" :> "5" :> Get '[JSON] Text
    , static6 :: mode :- "static" :> "6" :> Get '[JSON] Text
    , static7 :: mode :- "static" :> "7" :> Get '[JSON] Text
    , static8 :: mode :- "static" :> "8" :> Get '[JSON] Text
    } deriving Generic

mainServer :: Server MainAPI
mainServer = API
    { static1 = foo
    , static2 = foo
    , static3 = foo
    , static4 = foo
    , static5 = foo
    , static6 = foo
    , static7 = foo
    , static8 = foo
    }

foo = return "foo"

main :: IO ()
main = runSettings defaultSettings $
    serveWithContext (Proxy :: Proxy MainAPI) EmptyContext mainServer
