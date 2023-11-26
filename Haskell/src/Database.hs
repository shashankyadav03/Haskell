-- src/Database.hs
{-# LANGUAGE OverloadedStrings #-}

module Database
  ( connectDB,
    getAllPersons,
    insertPerson,
    deletePerson,
    Person (..),
  )
where

import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow

data Person = Person
  { personId :: Int,
    personName :: String,
    personEmail :: String
  }
  deriving (Show)

instance FromRow Person where
  fromRow = Person <$> field <*> field <*> field

connectDB :: IO Connection
connectDB = do
  conn <- open "test.db"
  execute_ conn "CREATE TABLE IF NOT EXISTS users1 (id INTEGER PRIMARY KEY, username TEXT NOT NULL, email TEXT NOT NULL)"
  return conn

getAllPersons :: Connection -> IO [Person]
getAllPersons conn = query_ conn "SELECT * FROM users1"

insertPerson :: Connection -> String -> String -> IO ()
insertPerson conn name email =
  execute conn "INSERT INTO users1 (username, email) VALUES (?, ?)" (name, email)

deletePerson :: Connection -> Int -> IO ()
deletePerson conn personId =
  execute conn "DELETE FROM users1 WHERE id = ?" (Only personId)
