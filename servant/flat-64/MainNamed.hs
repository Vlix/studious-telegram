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
    , static9 :: mode :- "static" :> "9" :> Get '[JSON] Text
    , static10 :: mode :- "static" :> "10" :> Get '[JSON] Text
    , static11 :: mode :- "static" :> "11" :> Get '[JSON] Text
    , static12 :: mode :- "static" :> "12" :> Get '[JSON] Text
    , static13 :: mode :- "static" :> "13" :> Get '[JSON] Text
    , static14 :: mode :- "static" :> "14" :> Get '[JSON] Text
    , static15 :: mode :- "static" :> "15" :> Get '[JSON] Text
    , static16 :: mode :- "static" :> "16" :> Get '[JSON] Text
    , static17 :: mode :- "static" :> "17" :> Get '[JSON] Text
    , static18 :: mode :- "static" :> "18" :> Get '[JSON] Text
    , static19 :: mode :- "static" :> "19" :> Get '[JSON] Text
    , static20 :: mode :- "static" :> "20" :> Get '[JSON] Text
    , static21 :: mode :- "static" :> "21" :> Get '[JSON] Text
    , static22 :: mode :- "static" :> "22" :> Get '[JSON] Text
    , static23 :: mode :- "static" :> "23" :> Get '[JSON] Text
    , static24 :: mode :- "static" :> "24" :> Get '[JSON] Text
    , static25 :: mode :- "static" :> "25" :> Get '[JSON] Text
    , static26 :: mode :- "static" :> "26" :> Get '[JSON] Text
    , static27 :: mode :- "static" :> "27" :> Get '[JSON] Text
    , static28 :: mode :- "static" :> "28" :> Get '[JSON] Text
    , static29 :: mode :- "static" :> "29" :> Get '[JSON] Text
    , static30 :: mode :- "static" :> "30" :> Get '[JSON] Text
    , static31 :: mode :- "static" :> "31" :> Get '[JSON] Text
    , static32 :: mode :- "static" :> "32" :> Get '[JSON] Text
    , static33 :: mode :- "static" :> "33" :> Get '[JSON] Text
    , static34 :: mode :- "static" :> "34" :> Get '[JSON] Text
    , static35 :: mode :- "static" :> "35" :> Get '[JSON] Text
    , static36 :: mode :- "static" :> "36" :> Get '[JSON] Text
    , static37 :: mode :- "static" :> "37" :> Get '[JSON] Text
    , static38 :: mode :- "static" :> "38" :> Get '[JSON] Text
    , static39 :: mode :- "static" :> "39" :> Get '[JSON] Text
    , static40 :: mode :- "static" :> "40" :> Get '[JSON] Text
    , static41 :: mode :- "static" :> "41" :> Get '[JSON] Text
    , static42 :: mode :- "static" :> "42" :> Get '[JSON] Text
    , static43 :: mode :- "static" :> "43" :> Get '[JSON] Text
    , static44 :: mode :- "static" :> "44" :> Get '[JSON] Text
    , static45 :: mode :- "static" :> "45" :> Get '[JSON] Text
    , static46 :: mode :- "static" :> "46" :> Get '[JSON] Text
    , static47 :: mode :- "static" :> "47" :> Get '[JSON] Text
    , static48 :: mode :- "static" :> "48" :> Get '[JSON] Text
    , static49 :: mode :- "static" :> "49" :> Get '[JSON] Text
    , static50 :: mode :- "static" :> "50" :> Get '[JSON] Text
    , static51 :: mode :- "static" :> "51" :> Get '[JSON] Text
    , static52 :: mode :- "static" :> "52" :> Get '[JSON] Text
    , static53 :: mode :- "static" :> "53" :> Get '[JSON] Text
    , static54 :: mode :- "static" :> "54" :> Get '[JSON] Text
    , static55 :: mode :- "static" :> "55" :> Get '[JSON] Text
    , static56 :: mode :- "static" :> "56" :> Get '[JSON] Text
    , static57 :: mode :- "static" :> "57" :> Get '[JSON] Text
    , static58 :: mode :- "static" :> "58" :> Get '[JSON] Text
    , static59 :: mode :- "static" :> "59" :> Get '[JSON] Text
    , static60 :: mode :- "static" :> "60" :> Get '[JSON] Text
    , static61 :: mode :- "static" :> "61" :> Get '[JSON] Text
    , static62 :: mode :- "static" :> "62" :> Get '[JSON] Text
    , static63 :: mode :- "static" :> "63" :> Get '[JSON] Text
    , static64 :: mode :- "static" :> "64" :> Get '[JSON] Text
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
    , static9 = foo
    , static10 = foo
    , static11 = foo
    , static12 = foo
    , static13 = foo
    , static14 = foo
    , static15 = foo
    , static16 = foo
    , static17 = foo
    , static18 = foo
    , static19 = foo
    , static20 = foo
    , static21 = foo
    , static22 = foo
    , static23 = foo
    , static24 = foo
    , static25 = foo
    , static26 = foo
    , static27 = foo
    , static28 = foo
    , static29 = foo
    , static30 = foo
    , static31 = foo
    , static32 = foo
    , static33 = foo
    , static34 = foo
    , static35 = foo
    , static36 = foo
    , static37 = foo
    , static38 = foo
    , static39 = foo
    , static40 = foo
    , static41 = foo
    , static42 = foo
    , static43 = foo
    , static44 = foo
    , static45 = foo
    , static46 = foo
    , static47 = foo
    , static48 = foo
    , static49 = foo
    , static50 = foo
    , static51 = foo
    , static52 = foo
    , static53 = foo
    , static54 = foo
    , static55 = foo
    , static56 = foo
    , static57 = foo
    , static58 = foo
    , static59 = foo
    , static60 = foo
    , static61 = foo
    , static62 = foo
    , static63 = foo
    , static64 = foo
    }

foo = return "foo"

main :: IO ()
main = runSettings defaultSettings $
    serveWithContext (Proxy :: Proxy MainAPI) EmptyContext mainServer
