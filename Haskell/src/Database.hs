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

data Legend = Legend { rank :: String, name :: String, village :: String } deriving (Show)

instance FromRow Legend where
  fromRow = Legend <$> field <*> field <*> field

-- Add FromNamedRecord and ToRow instances
instance FromNamedRecord Legend where
  parseNamedRecord r = Legend <$> r .: "Rank" <*> r .: "Name" <*> r .: "Village"

instance ToRow Legend where
  toRow (Legend a b c) = toRow (a, b, c)

-- Connect to SQLite database, create table, and insert data
connectAndCreateTable :: IO ()
connectAndCreateTable = do
  conn <- open "testcsv.db"
  execute_ conn "DROP TABLE IF EXISTS konoha"
  execute_ conn "CREATE TABLE IF NOT EXISTS konoha (Rank TEXT, Name TEXT, Village TEXT)"
  insertDataFromCSV conn
  queryTableContent conn
  close conn

-- Insert data from CSV into the table
insertDataFromCSV :: Connection -> IO ()
insertDataFromCSV conn = do
  csvData <- BL.readFile "testcsvdb.csv"
  case decodeByName csvData of
    Left err -> putStrLn $ "Error decoding CSV: " ++ err
    Right (header, dataRows) -> do
      putStrLn "Decoded data:"
      print (dataRows :: V.Vector Legend)
      executeMany conn "INSERT INTO konoha (Rank, Name, Village) VALUES (?, ?, ?)" (V.toList (dataRows :: V.Vector Legend))

-- Query and print the contents of the table
queryTableContent :: Connection -> IO ()
queryTableContent conn = do
  putStrLn "Table content after insertion:"
  rows <- query_ conn "SELECT * FROM konoha" :: IO [Legend]
  mapM_ print rows
