{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Prelude                    ()
import           Prelude.Compat

import           Control.Monad.Trans.Either
import           Data.GeoIP2
import           Data.IP
import qualified Data.Text                  as T
import           LocationAPI
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Servant
import           System.IO

type Handler = EitherT ServantErr IO

server :: GeoDB -> Server LocationAPI
server = location
  where location :: GeoDB -> IP -> Handler Country
        location db ip = return Country { countryCode = T.unpack code} where
          code
            | Right GeoResult { geoCountryISO = Just ciso } <- findGeoData db "en" ip = ciso
            | otherwise = "Unknown"

app :: GeoDB -> Application
app db = serve locationAPI (server db)

main :: IO ()
main = do
  db <- openGeoDB "geolite2-country.mmdb"
  run 8081 (app db)
