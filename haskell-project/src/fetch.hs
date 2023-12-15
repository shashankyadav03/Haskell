{-# LANGUAGE OverloadedStrings #-}

module Fetch (fetchDataAndProcess)where

import Network.HTTP.Conduit (simpleHttp, HttpException)
import Data.ByteString.Lazy as B
import Data.Aeson (eitherDecode, FromJSON)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Except (runExceptT, ExceptT, throwError)
import Control.Exception (try, IOException)

key :: String
key = "ff3e5ba7f0131d75e2e2b74eead4f697"

fetchWeatherData :: String  -> ExceptT String IO B.ByteString
fetchWeatherData location = do
    let apiUrl = "http://api.weatherstack.com/current?access_key=" ++ key ++ "&query=" ++ location
    response <- liftIO $ try (simpleHttp apiUrl) :: ExceptT String IO (Either HttpException B.ByteString)
    case response of
        Right body -> return body
        Left err -> throwError $ "HTTP request failed: " ++ show err
    

-- Function to get Data from API
fetchDataAndProcess :: String -> IO (Either String ())
fetchDataAndProcess location = do
    putStrLn "Fetching Data from API......"
    result <- runExceptT $ fetchWeatherData location
    case result of
        Right response -> do
            putStrLn "Data Fetched from API!"
            putStrLn "Saving Fetched data to file....."
            writeFileResult <- try (B.writeFile "data/response.json" response) :: IO (Either IOException ())
            case writeFileResult of
                Right _ -> do
                    putStrLn "\nWeather data saved successfully!."
                    return $ Right ()
                Left e -> do
                    putStrLn $ "\nFailed to write weather data to file: " ++ show e
                    return $ Left "File writing failed"
        Left errorMsg -> do
            putStrLn $ "\nFailed to fetch weather data: " ++ errorMsg
            return $ Left "Fetch failed"