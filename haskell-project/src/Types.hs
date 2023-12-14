--Type.hs
{-# LANGUAGE DeriveGeneric #-}

module Types
  ( WeatherRecord(..)
  , WeatherInfo(..)
  , CityInfo(..)
  , ActivityInfo(..)
  , Request(..)
  , Location(..)
  , Current(..)
  , WeatherData(..)
  ) where

import GHC.Generics

data CityInfo = CityInfo 
  { sr_no :: Int, 
  city :: String, 
  mintemp_maxtemp:: String ,
  mintemp :: Int, 
  maxtemp :: Int, 
  rain::String, 
  activity_id::String
  } deriving (Show)

data ActivityInfo = ActivityInfo 
  { activityId :: String, 
  activities :: String 
  } deriving (Show)

data WeatherRecord = WeatherRecord
  { wrRequestType :: String
  , wrQuery :: String
  , wrLanguage :: String
  , wrUnit :: String
  , wrLocationName :: String
  , wrCountry :: String
  , wrRegion :: String
  , wrLat :: String
  , wrLon :: String
  , wrTimezoneId :: String
  , wrLocaltime :: String
  , wrLocaltimeEpoch :: Integer
  , wrUtcOffset :: String
  , wrObservationTime :: String
  , wrTemperature :: Float
  , wrWeatherCode :: Int
  , wrWeatherIcons :: String
  , wrWeatherDescriptions :: String
  , wrWindSpeed :: Int
  , wrWindDegree :: Int
  , wrWindDir :: String
  , wrPressure :: Int
  , wrPrecip :: Float
  , wrHumidity :: Int
  , wrCloudcover :: Int
  , wrFeelslike :: Int
  , wrUvIndex :: Int
  , wrVisibility :: Int
  , wrIsDay :: String
  } deriving (Show)

data WeatherInfo = WeatherInfo
  { exlocation :: String
  , exprecipitation :: Float
  , extemperature :: Float
  } deriving (Show)

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
