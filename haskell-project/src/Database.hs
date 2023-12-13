-- Database.hs
{-# LANGUAGE OverloadedStrings #-}

module Database
  ( connectAndCreateTable
  , insertCityDataFromCSV
  , insertActivityInfoFromCSV
  , queryCityInfoTable
  , queryActivityInfoTable
  ) where

import Database.SQLite.Simple
import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector as V
import Data.Csv
import Data.List.Split (splitOn)
import Data.List (intercalate)
import Control.Monad (forM_)

data CityInfo = CityInfo { sr_no :: Int, city :: String, mintemp_maxtemp:: String ,mintemp :: Int, maxtemp :: Int, rain::String, activity_id::String} deriving (Show)
data ActivityInfo = ActivityInfo { activityId :: String, activities :: String } deriving (Show)

instance FromRow CityInfo where
  fromRow = CityInfo <$> field <*> field <*> field <*> field <*> field <*> field <*> field
instance FromRow ActivityInfo where
  fromRow = ActivityInfo <$> field <*> field

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
connectAndCreateTable :: String -> String -> Float -> IO ()
connectAndCreateTable cityName rainStatus tempResult = do
  conn <- open "testcsv.db"
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