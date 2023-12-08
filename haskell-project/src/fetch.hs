--function to fetch weather data from openweathermap.org in haskell
module Fetch where

import Network.HTTP.Conduit (simpleHttp)
import Data.ByteString.Lazy as B
import Data.Aeson (eitherDecode, FromJSON)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Except (runExceptT, ExceptT, throwError)

-- secert key, only 1000 calls per month
key = "ff3e5ba7f0131d75e2e2b74eead4f697"
-- This is the function to be called from Main.hs.
fetchWeatherData :: String  -> ExceptT String IO B.ByteString
fetchWeatherData location = do
    let apiUrl = "http://api.weatherstack.com/current?access_key=" ++ key ++ "&query=" ++ location
    response <- liftIO $ simpleHttp apiUrl
    return response
