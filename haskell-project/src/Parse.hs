{-# LANGUAGE DeriveGeneric #-}

module Parse where

import GHC.Generics
import Data.Aeson
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL

-- Define data types to represent the JSON structure
data Request = Request
  { requestType :: String
  , query :: String
  , language :: String
  , unit :: String
  } deriving (Generic, Show)

data Location = Location
  { name :: String
  , country :: String
  , region :: String
  , lat :: String
  , lon :: String
  , timezone_id :: String
  , localtime :: String
  , localtime_epoch :: Integer
  , utc_offset :: String
  } deriving (Generic, Show)

data Current = Current
  { observation_time :: String
  , temperature :: Int
  , weather_code :: Int
  , weather_icons :: [String]
  , weather_descriptions :: [String]
  , wind_speed :: Int
  , wind_degree :: Int
  , wind_dir :: String
  , pressure :: Int
  , precip :: Int
  , humidity :: Int
  , cloudcover :: Int
  , feelslike :: Int
  , uv_index :: Int
  , visibility :: Int
  , is_day :: String
  } deriving (Generic, Show)

data WeatherData = WeatherData
  { request :: Request
  , location :: Location
  , current :: Current
  } deriving (Generic, Show)

-- Instances for automatic derivation
instance FromJSON Request
instance FromJSON Location
instance FromJSON Current
instance FromJSON WeatherData

parseWeatherData' :: String -> IO ()
parseWeatherData' filePath = do
    -- Read the JSON file
    jsonContents <- B.readFile filePath

    -- Convert strict ByteString to lazy ByteString
    let lazyJsonContents = BL.fromStrict jsonContents

    -- Parse JSON
    case eitherDecode lazyJsonContents of
        Left err -> putStrLn $ "Error decoding JSON: " ++ err
        Right weatherData -> do
            -- Print or process the parsed data as needed
            putStrLn $ "Temperature: " ++ show (temperature (current weatherData))
            putStrLn $ "Precipitation: " ++ show (precip (current weatherData))

parse :: IO ()
parse = parseWeatherData' "C:/Users/athar/OneDrive/Documents/WeatherWander/haskell-project/data/response.json"
