{-# LANGUAGE OverloadedStrings #-}

import Data.GeoIP2
import Data.IP (IP(..))

main = do
  db <- openGeoDB "geolite2-country.mmdb"
  let ip = IPv4 "151.101.84.144"
  print $ findGeoData db "da" ip
