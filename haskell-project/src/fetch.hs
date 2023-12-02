--function to fetch weather data from openweathermap.org in haskell
module Fetch (fetchWeatherData'', WeatherData ) where

-- import Network.HTTP.Conduit (simpleHttp)
-- import Data.ByteString.Lazy as B
-- import Data.Aeson (eitherDecode, FromJSON)
import Control.Monad.Trans.Except (runExceptT, throwE)
import Control.Monad.IO.Class (liftIO)

-- | A type to represent the relevant weather data.
data WeatherData = WeatherData {
    temperature :: Double,
    rainChance :: Double
    -- Add more fields as needed
} deriving (Show)

-- -- Make sure WeatherData is an instance of FromJSON to parse the JSON response
-- instance FromJSON WeatherData where
--     -- Implement JSON parsing according to the structure of your API response
-- API_KEY = "Use response.json meanwhile"
-- -- | Fetches weather data for a given city from the weather API.
-- fetchWeatherData :: String -> IO (Either String WeatherData)
-- fetchWeatherData city = do
--     let apiUrl = "http://api.weatherstack.com/current?access_key=" ++ API_KEY ++"&query="++ city
--     response <- simpleHttp apiUrl
--     return $ eitherDecode response :: IO (Either String WeatherData)

-- function to use response.json as a response
-- fetchWeatherData' :: String -> IO (Either String WeatherData)
-- fetchWeatherData' city = do
--     response <- B.readFile "response.json"
--     return $ eitherDecode response :: IO (Either String WeatherData)


fetchWeatherData'' :: String -> IO (Either String WeatherData)
fetchWeatherData'' city = runExceptT $ do
    -- Simulate fetching data by location (for demonstration purposes)
    weatherData <- case city of
        "NewYork" -> return $ WeatherData 25.5 12.5
        "Paris"   -> return $ WeatherData 15.0 50.75
        "London"   -> return $ WeatherData 5.0 70.25
        _         -> throwE "Location not found"
    
    -- Simulate an API call or database query (for demonstration purposes)
    liftIO $ putStrLn $ "Fetching weather data for " ++ city ++ "..."
    return weatherData