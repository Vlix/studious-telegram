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
    { static :: mode :- "static" :> "foo" :> Get '[JSON] Text
    } deriving Generic

mainServer :: Server MainAPI
mainServer = foo

foo = API
    { static = return "foo"
    }

main :: IO ()
main = runSettings defaultSettings $
    serveWithContext (Proxy :: Proxy MainAPI) EmptyContext mainServer
