{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module LocationAPI where

import           Data.Aeson.Compat
import           Data.IP
import qualified Data.Text         as T
import           GHC.Generics
import           Servant
import           Text.Read
import           Web.HttpApiData

type LocationAPI = "location" :> Capture "ip" IP :> Get '[JSON] Country

data Country = Country
  { countryCode :: String }
  deriving (Generic, Show)

instance ToJSON Country

instance FromHttpApiData IP where
  parseUrlPiece text
    | Just ip <- readMaybe $ T.unpack text = Right ip
    | otherwise = Left "Not a parseable IP address"

instance FromText IP where
  fromText text
    | Right ip <- parseUrlPiece text = Just ip
    | otherwise = Nothing

locationAPI :: Proxy LocationAPI
locationAPI = Proxy
