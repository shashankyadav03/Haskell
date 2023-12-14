-- Database.hs
{-# LANGUAGE OverloadedStrings #-}

module Database
  ( WeatherRecord(..)
  , WeatherInfo(..)
  , connectAndCreateTable
  , connectDB
  , insertCityDataFromCSV
  , insertActivityInfoFromCSV
  , insertOrUpdateWeather
  , queryCityInfoTable
  , queryActivityInfoTable
  , fetchWeatherInfo
  ) where

import Database.SQLite.Simple
import Database.SQLite.Simple.ToField
import Control.Exception (bracket)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector as V
import Data.Csv
import Data.List.Split (splitOn)
import Data.List (intercalate)
import Control.Monad (forM_)
import Data.Maybe (listToMaybe)
import Types(WeatherRecord(..), WeatherInfo(..), CityInfo(..), ActivityInfo(..))

instance FromRow CityInfo where
  fromRow = CityInfo <$> field <*> field <*> field <*> field <*> field <*> field <*> field
instance FromRow ActivityInfo where
  fromRow = ActivityInfo <$> field <*> field
instance FromRow WeatherRecord where
  fromRow = WeatherRecord <$> field <*> field <*> field <*> field <*> field <*> field
                          <*> field <*> field <*> field <*> field <*> field <*> field
                          <*> field <*> field <*> field <*> field <*> field <*> field
                          <*> field <*> field <*> field <*> field <*> field <*> field
                          <*> field <*> field <*> field <*> field <*> field
instance FromRow WeatherInfo where
  fromRow = WeatherInfo <$> field <*> field <*> field

-- Add FromNamedRecord and ToRow instances
instance FromNamedRecord CityInfo where
  parseNamedRecord r = CityInfo <$> r .: "Sr_No" <*> r .: "City" <*> r .: "MinTemp_MaxTemp" <*> r .: "MinTemp" <*> r .: "MaxTemp" <*> r .: "Rain" <*> r .: "Activity_Id"
instance FromNamedRecord ActivityInfo where
  parseNamedRecord r = ActivityInfo <$> r .: "Activity_Id" <*> r .: "Activities"

instance ToRow CityInfo where
  toRow (CityInfo a b c d e f g) = toRow (a, b, c, d, e, f, g)
instance ToRow ActivityInfo where
  toRow (ActivityInfo a b) = toRow (a, b)

-- Connect to SQLite database, create table, and insert data
connectAndCreateTable :: Connection -> String -> String -> Float -> IO ()
connectAndCreateTable conn cityName rainStatus tempResult = do
  -- conn <- open "testcsv.db"
  execute_ conn "DROP TABLE IF EXISTS city_info"
  execute_ conn "DROP TABLE IF EXISTS activity_info"
  execute_ conn "CREATE TABLE IF NOT EXISTS activity_info (Activity_Id TEXT PRIMARY KEY, Activities TEXT)"
  execute_ conn "CREATE TABLE IF NOT EXISTS city_info (Sr_No INTEGER PRIMARY KEY, City TEXT, MinTemp_MaxTemp TEXT, MinTemp INTEGER, MaxTemp INTEGER, Rain TEXT, Activity_Id TEXT, FOREIGN KEY (Activity_Id) REFERENCES activity_info(Activity_Id))"
  insertCityDataFromCSV conn
  insertActivityInfoFromCSV conn
  result1 <- queryCityInfoTable conn cityName rainStatus tempResult
  forM_ result1 $ \(cityResult, activityIdResult) -> do
    activitiesStr <- queryActivityInfoTable conn activityIdResult
    putStrLn $ "\nCity: " ++ cityResult
    putStrLn "\nActivities: " 
    let activityList = splitOn ", " activitiesStr
    mapM_ putStrLn activityList
  close conn

connectDB :: IO Connection
connectDB = open "testcsv.db"

insertOrUpdateWeather :: Connection -> WeatherRecord -> IO ()
insertOrUpdateWeather conn record = do
    -- Check if the city already exists
    execute_ conn "CREATE TABLE IF NOT EXISTS weather_info (\
                   \wrRequestType TEXT, wrQuery TEXT, wrLanguage TEXT, wrUnit TEXT, \
                   \wrLocationName TEXT, wrCountry TEXT, wrRegion TEXT, wrLat TEXT, wrLon TEXT, \
                   \wrTimezoneId TEXT, wrLocaltime TEXT, wrLocaltimeEpoch INTEGER, wrUtcOffset TEXT, \
                   \wrObservationTime TEXT, wrTemperature REAL, wrWeatherCode INTEGER, \
                   \wrWeatherIcons TEXT, wrWeatherDescriptions TEXT, wrWindSpeed INTEGER, \
                   \wrWindDegree INTEGER, wrWindDir TEXT, wrPressure INTEGER, wrPrecip REAL, \
                   \wrHumidity INTEGER, wrCloudcover INTEGER, wrFeelslike INTEGER, \
                   \wrUvIndex INTEGER, wrVisibility INTEGER, wrIsDay TEXT)"
    cityExists <- query conn "SELECT EXISTS(SELECT 1 FROM weather_info WHERE wrLocationName = ? LIMIT 1)" (Only (wrLocationName record)) :: IO [Only Int]
    let params = [ ":rtype" := wrRequestType record
                 , ":qry" := wrQuery record
                 , ":lang" := wrLanguage record
                 , ":unit" := wrUnit record
                 , ":locname" := wrLocationName record
                 , ":country" := wrCountry record
                 , ":region" := wrRegion record
                 , ":lat" := wrLat record
                 , ":lon" := wrLon record
                 , ":tzid" := wrTimezoneId record
                 , ":ltime" := wrLocaltime record
                 , ":ltimeEpoch" := wrLocaltimeEpoch record
                 , ":utcOff" := wrUtcOffset record
                 , ":obsTime" := wrObservationTime record
                 , ":temp" := wrTemperature record
                 , ":wcode" := wrWeatherCode record
                 , ":wicons" := wrWeatherIcons record
                 , ":wdesc" := wrWeatherDescriptions record
                 , ":wspeed" := wrWindSpeed record
                 , ":wdeg" := wrWindDegree record
                 , ":wdir" := wrWindDir record
                 , ":press" := wrPressure record
                 , ":prec" := wrPrecip record
                 , ":hum" := wrHumidity record
                 , ":cloud" := wrCloudcover record
                 , ":feels" := wrFeelslike record
                 , ":uv" := wrUvIndex record
                 , ":vis" := wrVisibility record
                 , ":day" := wrIsDay record
                 ]
    case cityExists of
        [Only 1] -> do
            -- Update the existing record
            executeNamed conn "UPDATE weather_info SET \
                               \wrRequestType = :rtype, wrQuery = :qry, wrLanguage = :lang, wrUnit = :unit, \
                               \wrCountry = :country, wrRegion = :region, wrLat = :lat, wrLon = :lon, wrTimezoneId = :tzid, \
                               \wrLocaltime = :ltime, wrLocaltimeEpoch = :ltimeEpoch, wrUtcOffset = :utcOff, \
                               \wrObservationTime = :obsTime, wrTemperature = :temp, wrWeatherCode = :wcode, \
                               \wrWeatherIcons = :wicons, wrWeatherDescriptions = :wdesc, wrWindSpeed = :wspeed, \
                               \wrWindDegree = :wdeg, wrWindDir = :wdir, wrPressure = :press, wrPrecip = :prec, \
                               \wrHumidity = :hum, wrCloudcover = :cloud, wrFeelslike = :feels, \
                               \wrUvIndex = :uv, wrVisibility = :vis, wrIsDay = :day \
                               \WHERE wrLocationName = :locname" params
            putStrLn "Current City Updated!!"
        [Only 0] -> do
            -- Insert new record
            executeNamed conn "INSERT INTO weather_info (wrRequestType, wrQuery, wrLanguage, wrUnit, \
                               \wrLocationName, wrCountry, wrRegion, wrLat, wrLon, wrTimezoneId, \
                               \wrLocaltime, wrLocaltimeEpoch, wrUtcOffset, \
                               \wrObservationTime, wrTemperature, wrWeatherCode, \
                               \wrWeatherIcons, wrWeatherDescriptions, wrWindSpeed, \
                               \wrWindDegree, wrWindDir, wrPressure, wrPrecip, \
                               \wrHumidity, wrCloudcover, wrFeelslike, \
                               \wrUvIndex, wrVisibility, wrIsDay) \
                               \VALUES (:rtype, :qry, :lang, :unit, :locname, :country, :region, :lat, :lon, :tzid, :ltime, :ltimeEpoch, :utcOff, :obsTime, :temp, :wcode, :wicons, :wdesc, :wspeed, :wdeg, :wdir, :press, :prec, :hum, :cloud, :feels, :uv, :vis, :day)" params
            putStrLn "New City Inserted!!"
        _ -> putStrLn "Error: Unexpected result when checking for existing city"
    -- records <- query_ conn "SELECT * FROM weather_info" :: IO [WeatherRecord] 
    -- mapM_ print records


fetchWeatherInfo :: Connection -> String -> IO (Maybe WeatherInfo)
fetchWeatherInfo conn locName = do
    result <- query conn "SELECT wrLocationName, wrPrecip, wrTemperature FROM weather_info WHERE wrLocationName = ? LIMIT 1" (Only locName) :: IO [WeatherInfo]
    return $ listToMaybe result  -- Converts the list to Maybe, Nothing if the list is empty

-- Insert City data from CSV into the table
insertCityDataFromCSV :: Connection -> IO ()
insertCityDataFromCSV conn = do
  csvData <- BL.readFile "cityInfo.csv"
  case decodeByName csvData of
    Left err -> putStrLn $ "Error decoding CSV: " ++ err
    Right (header, dataRows) -> do
      executeMany conn "INSERT INTO city_info (Sr_No, City, MinTemp_MaxTemp, MinTemp, MaxTemp, Rain, Activity_Id) VALUES (?, ?, ?, ?, ?, ?, ?)" (V.toList (dataRows :: V.Vector CityInfo))

-- Insert Activity data from CSV into the table
insertActivityInfoFromCSV :: Connection -> IO ()
insertActivityInfoFromCSV conn = do
  csvDataActivity <- BL.readFile "activityInfo.csv"
  case decodeByName csvDataActivity of
    Left err -> putStrLn $ "Error decoding CSV: " ++ err
    Right (_, dataRows) ->
      executeMany conn "INSERT INTO activity_info (Activity_Id, Activities) VALUES (?, ?)" (V.toList (dataRows :: V.Vector ActivityInfo))

-- Query and return the contents of the table
queryCityInfoTable :: Connection -> String -> String -> Float -> IO [(String, String)]
queryCityInfoTable conn cityName rainStatus tempResult = do
    let sql = "SELECT City, Activity_Id FROM city_info WHERE City = ? AND Rain = ? AND MinTemp <= ? AND MaxTemp >= ?"
    rows <- query conn sql (cityName, rainStatus, tempResult, tempResult) :: IO [(String, String)]
    return rows

-- Query and return the contents of the 'activity_info' table
queryActivityInfoTable :: Connection -> String -> IO String
queryActivityInfoTable conn activityIdResult = do
    let sql = "SELECT Activities FROM activity_info WHERE Activity_Id = ?"
    rows <- query conn sql (Only activityIdResult) :: IO [Only String]
    return $ intercalate ", " (map fromOnly rows)