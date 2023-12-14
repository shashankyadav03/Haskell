--Main.hs
module Main(main, getLocation, fetchDataAndProcess) where

import Fetch (fetchWeatherData)
import Parse (parse)
import System.Environment (getArgs)
import Control.Monad.Except (runExceptT)
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.ByteString.Lazy as B
import Control.Exception (try, IOException)
import Data.Time.Clock
import Data.Time.Format
import Database (connectDB, getDbTime)

getLocation :: [String] -> String
getLocation args = case args of
    [] -> "London"  -- Default to London if no arguments are provided
    [loc] -> loc    -- Use the provided argument
    _ -> "\nUsage: WeatherWander <LOCATION>"

fetchDataAndProcess :: String -> IO ()
fetchDataAndProcess location = do
    result <- runExceptT $ fetchWeatherData location
    case result of
        Right response -> do
            putStrLn "Data Fetched from API!"
            putStrLn "Saving Fetched data to file....."
            writeFileResult <- try (B.writeFile "data/response.json" response) :: IO (Either IOException ())
            case writeFileResult of
                Right _ -> putStrLn "\nWeather data saved successfully!."
                Left e -> putStrLn $ "\nFailed to write weather data to file: " ++ show e
        Left errorMsg -> putStrLn $ "\nFailed to fetch weather data: " ++ errorMsg

main :: IO ()
main = do
    putStrLn "\nStarting WeatherWander......."
    args <- getArgs
    let location = getLocation args
    currentTime <- getCurrentTime
    let currentTimeStr = formatTime defaultTimeLocale "%Y-%m-%d %H:%M" currentTime
    conn <- connectDB
    maybeDbTime <- getDbTime conn location  -- Replace with the actual location name
    case maybeDbTime of
        Just dbTimeString -> do
            putStrLn $ "DB Time: " ++ dbTimeString
            putStrLn $ "Current Time: " ++ currentTimeStr
            let maybeDbTime = parseTimeM True defaultTimeLocale "%Y-%m-%d %H:%M" dbTimeString :: Maybe UTCTime
            case maybeDbTime of
                Just dbTime -> do
                    let timeDiff = diffUTCTime currentTime dbTime
                    if timeDiff < secondsToNominalDiffTime 3600 -- 3600 seconds = 1 hour
                        then do
                            let val = 1 
                            putStrLn "API time is within the last hour\nExtracting Data from Database....."
                            parse val location-- assuming parse is an IO action
                        else do
                            let val = 0
                            putStrLn "API time is more than an hour ago\nFetching Data from API......"
                            fetchDataAndProcess location -- replace with actual logic
                            parse val location-- assuming parse is an IO action
                Nothing -> putStrLn "Failed to parse API time"
        Nothing -> putStrLn "No time data found for the specified location"