{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

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
  , temperature :: Float
  , weather_code :: Int
  , weather_icons :: [String]
  , weather_descriptions :: [String]
  , wind_speed :: Int
  , wind_degree :: Int
  , wind_dir :: String
  , pressure :: Int
  , precip :: Float
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
instance FromJSON Request where
    parseJSON = withObject "Request" $ \v -> Request
        <$> v .: "type"
        <*> v .: "query"
        <*> v .: "language"
        <*> v .: "unit"
instance FromJSON Location
instance FromJSON Current
instance FromJSON WeatherData

-- Determine rain status based on precipitation value
getRainStatus :: Float -> String
getRainStatus precipValue
    | precipValue < 50 = "no"
    | otherwise = "yes"

parseWeatherData :: String -> IO (String, Float, Float, String)
parseWeatherData filePath = do
    -- Read the JSON file
    jsonContents <- B.readFile filePath

    -- Convert strict ByteString to lazy ByteString
    let lazyJsonContents = BL.fromStrict jsonContents

    -- Parse JSON
    case eitherDecode lazyJsonContents of
        Left err -> do
            putStrLn $ "Error decoding JSON: " ++ err
            return ("0",0, 0, "0")  -- Return default values or handle the error as needed
        Right weatherData -> do
            -- Print or process the parsed data as needed
            let place = name (location weatherData)
                temp = temperature (current weatherData)
                preci = precip (current weatherData)
                rainStatus = getRainStatus preci
            return (place, temp, preci, rainStatus)

parse :: IO (String, Float, String)
parse = do
    (place, temp, preci, rainStatus) <- parseWeatherData "data/response.json"
    return (place,temp,rainStatus) 
