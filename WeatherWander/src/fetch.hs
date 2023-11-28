--function to fetch weather data from openweathermap.org in haskell
module Fetch where

import Network.HTTP.Conduit (simpleHttp)
import Data.ByteString.Lazy as B
import Data.Aeson (eitherDecode, FromJSON)

-- | A type to represent the relevant weather data.
data WeatherData = WeatherData {
    temperature :: Double,
    rainChance :: Double
    -- Add more fields as needed
} deriving (Show)

-- Make sure WeatherData is an instance of FromJSON to parse the JSON response
instance FromJSON WeatherData where
    -- Implement JSON parsing according to the structure of your API response
API_KEY = "Use response.json meanwhile"
-- | Fetches weather data for a given city from the weather API.
fetchWeatherData :: String -> IO (Either String WeatherData)
fetchWeatherData city = do
    let apiUrl = "http://api.weatherstack.com/current?access_key=" ++ API_KEY ++"&query="++ city
    response <- simpleHttp apiUrl
    return $ eitherDecode response :: IO (Either String WeatherData)
