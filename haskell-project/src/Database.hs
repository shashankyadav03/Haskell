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

data Legend = Legend { city :: String, mintemp :: Int, maxtemp :: Int, rain::String, activities::String} deriving (Show)

instance FromRow Legend where
  fromRow = Legend <$> field <*> field <*> field <*> field <*> field

-- Add FromNamedRecord and ToRow instances
instance FromNamedRecord Legend where
  parseNamedRecord r = Legend <$> r .: "City" <*> r .: "MinTemp" <*> r .: "MaxTemp" <*> r .: "Rain" <*> r .: "Activities"

instance ToRow Legend where
  toRow (Legend a b c d e) = toRow (a, b, c, d, e)

-- Connect to SQLite database, create table, and insert data
-- connectAndCreateTable to include dynamic parameters
connectAndCreateTable :: String -> String -> Int -> IO ()
connectAndCreateTable city rain temp = do
  conn <- open "testcsv.db"
  execute_ conn "DROP TABLE IF EXISTS activities"
  execute_ conn "CREATE TABLE IF NOT EXISTS activities (City TEXT, MinTemp NUM, MaxTemp NUM, Rain TEXT, Activities TEXT)"
  insertDataFromCSV conn
  queryTableContent conn city rain temp
  close conn

-- Insert data from CSV into the table
insertDataFromCSV :: Connection -> IO ()
insertDataFromCSV conn = do
  csvData <- BL.readFile "cityactivities.csv"
  case decodeByName csvData of
    Left err -> putStrLn $ "Error decoding CSV: " ++ err
    Right (header, dataRows) -> do
      --putStrLn "Decoded data:"
      --print (dataRows :: V.Vector Legend)
      executeMany conn "INSERT INTO activities (City, MinTemp, MaxTemp, Rain, Activities) VALUES (?, ?, ?, ?, ?)" (V.toList (dataRows :: V.Vector Legend))

-- Query and print the contents of the table
-- queryTableContent to accept dynamic parameters
queryTableContent :: Connection -> String -> String -> Int -> IO ()
queryTableContent conn city rain temp = do
  putStrLn "Table content after insertion:"
  rows <- query conn "SELECT * FROM activities WHERE City = ? AND Rain = ? AND MinTemp <= ? AND MaxTemp >= ?" (city, rain, temp, temp) :: IO [Legend]
  mapM_ print rows
