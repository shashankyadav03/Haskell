-- app/main.hs
module Main where

import Database
import Database.SQLite.Simple (close)

main :: IO ()
main = do
  conn <- connectDB

  putStrLn "1. Insert Person"
  putStrLn "2. Retrieve All Persons"
  putStrLn "4. Delete Person"
  putStrLn "5. Exit"

  putStrLn "Enter your choice:"
  choice <- getLine

  case choice of
    "1" -> do
      putStrLn "Enter name:"
      name <- getLine
      putStrLn "Enter email:"
      email <- getLine
      insertPerson conn name email
      putStrLn "Person inserted successfully!"
      main

    "2" -> do
      persons <- getAllPersons conn
      putStrLn "All Persons:"
      mapM_ print persons
      main

    "4" -> do
      putStrLn "Enter person ID to delete:"
      personIdStr <- getLine
      let personId = read personIdStr :: Int
      deletePerson conn personId
      putStrLn "Person deleted successfully!"
      main

    "5" -> do
      putStrLn "Exiting program..."
      close conn

    _ -> do
      putStrLn "Invalid choice. Please enter a valid option."
      main
