--Main.hs
module Main(main, getLocation, fetchDataAndProcess) where
-- Importing Modules
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
import Control.Monad (join)

-- Function get location
getLocation :: [String] -> String
getLocation args = case args of
    [] -> "London"  -- Default to London if no arguments are provided
    [loc] -> loc    
    _ -> "\nUsage: WeatherWander <LOCATION>"

-- Function to get Data from API
fetchDataAndProcess :: String -> IO ()
fetchDataAndProcess location = do
    putStrLn "Fetching Data from API......"
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

-- Main Function
main :: IO ()
main = do
    putStrLn "\nStarting WeatherWander......."
    args <- getArgs
    let location = getLocation args
    currentTime <- getCurrentTime   
    let currentTimeStr = formatTime defaultTimeLocale "%Y-%m-%d %H:%M" currentTime
    conn <- connectDB
    -- Applying fmap to parse the DB time string directly if it's present
    maybeDbTime <- join . fmap (parseTimeM True defaultTimeLocale "%Y-%m-%d %H:%M") <$> getDbTime conn location
    case maybeDbTime of
        Just dbTime -> do
            let dbTimeString = formatTime defaultTimeLocale "%Y-%m-%d %H:%M" dbTime
            putStrLn $ "DB Time: " ++ dbTimeString
            putStrLn $ "Current Time: " ++ currentTimeStr
            let timeDiff = diffUTCTime currentTime dbTime
            if timeDiff < secondsToNominalDiffTime 3600 -- 3600 seconds = 1 hour
                then do
                    putStrLn "API time is within the last hour\nExtracting Data from Database....."
                    parse 1 location
                else do
                    putStrLn "API time is more than an hour ago"
                    fetchDataAndProcess location 
                    parse 0 location
        Nothing -> do
            putStrLn "No data found for the specified location"
            fetchDataAndProcess location
            parse 0 location
