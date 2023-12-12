-- Database.hs
{-# LANGUAGE OverloadedStrings #-}

module Database
  ( connectAndCreateTable
  , insertDataFromCSV
  , queryTableContent
  ) where

import Database.SQLite.Simple
import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector as V
import Data.Char (ord)
import Data.Csv
import Data.List.Split (splitOn)
import Data.Char (isSpace)
import Data.List (dropWhileEnd)

data Legend = Legend { city :: String, mintemp_maxtemp:: String ,mintemp :: Int, maxtemp :: Int, rain::String, activities::String} deriving (Show)

instance FromRow Legend where
  fromRow = Legend <$> field <*> field <*> field <*> field <*> field <*> field

-- Add FromNamedRecord and ToRow instances
instance FromNamedRecord Legend where
  parseNamedRecord r = Legend <$> r .: "City" <*> r .: "MinTemp_MaxTemp" <*> r .: "MinTemp" <*> r .: "MaxTemp" <*> r .: "Rain" <*> r .: "Activities"

instance ToRow Legend where
  toRow (Legend a b c d e f) = toRow (a, b, c, d, e, f)

splitActivities :: String -> [String]
splitActivities = map trim . splitOn ","

trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace

-- Connect to SQLite database, create table, and insert data
-- connectAndCreateTable to include dynamic parameters
connectAndCreateTable :: String -> String -> Float -> IO ()
connectAndCreateTable city rain temp = do
  conn <- open "testcsv.db"
  execute_ conn "DROP TABLE IF EXISTS activities"
  execute_ conn "CREATE TABLE IF NOT EXISTS activities (City TEXT, MinTemp_MaxTemp TEXT, MinTemp NUM, MaxTemp NUM, Rain TEXT, Activities TEXT)"
  insertDataFromCSV conn
  result <- queryTableContent conn city rain temp
  case result of
    [(cityResult, activitiesResult)] -> do
      putStrLn $ "City: " ++ cityResult
      putStrLn "\nActivities:"
      mapM_ putStrLn (splitActivities activitiesResult)
    _ -> putStrLn "No matching data found"
  close conn

-- Insert data from CSV into the table
insertDataFromCSV :: Connection -> IO ()
insertDataFromCSV conn = do
  csvData <- BL.readFile "city_activities.csv"
  case decodeByName csvData of
    Left err -> putStrLn $ "Error decoding CSV: " ++ err
    Right (header, dataRows) -> do
      --putStrLn "Decoded data:"
      --print (dataRows :: V.Vector Legend)
      executeMany conn "INSERT INTO activities (City, MinTemp_MaxTemp, MinTemp, MaxTemp, Rain, Activities) VALUES (?, ?, ?, ?, ?, ?)" (V.toList (dataRows :: V.Vector Legend))

-- Query and print the contents of the table
-- queryTableContent to accept dynamic parameters
queryTableContent :: Connection -> String -> String -> Float -> IO [(String, String)]
queryTableContent conn city rain temp = do
  -- putStrLn "Table content after insertion:"
  rows <- query conn "SELECT City, Activities FROM activities WHERE City = ? AND Rain = ? AND MinTemp <= ? AND MaxTemp >= ?" (city, rain, temp, temp) :: IO [(String,String)]
  return rows
  
