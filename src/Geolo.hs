{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Main where

import           Prelude                    ()
import           Prelude.Compat

import           Control.Monad.Trans.Either
import           Data.Aeson.Compat
import           Data.GeoIP2
import           Data.IP
import qualified Data.Text                  as T
import           GHC.Generics
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Servant
import           System.IO
import           Text.Read
import           Web.HttpApiData

type LocationAPI = "location" :> Capture "ip" IP :> Get '[JSON] Country

data Country = Country
  { countryCode :: String }
  deriving (Generic, Show)

instance FromText IP where
  fromText text
    | Right ip <- parseUrlPiece text = Just ip
    | otherwise = Nothing

instance ToJSON Country

instance FromHttpApiData IP where
  parseUrlPiece text
    | Just ip <- readMaybe $ T.unpack text = Right ip
    | otherwise = Left "Not parseable as IP"

type Handler = EitherT ServantErr IO

server :: GeoDB -> Server LocationAPI
server = location
  where location :: GeoDB -> IP -> Handler Country
        location db ip = return Country { countryCode = T.unpack code} where
          code
            | Right GeoResult { geoCountryISO = Just ciso } <- findGeoData db "en" ip = ciso
            | otherwise = "Unknown"

locationAPI :: Proxy LocationAPI
locationAPI = Proxy

app :: GeoDB -> Application
app db = serve locationAPI (server db)

main :: IO ()
main = do
  db <- openGeoDB "geolite2-country.mmdb"
  run 8081 (app db)
