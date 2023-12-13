module Main where

import Fetch (fetchWeatherData)
import Database (connectAndCreateTable)
import Parse (parse)
import System.Environment (getArgs)
import Control.Monad.Except (runExceptT)
import qualified Data.ByteString.Lazy.Char8 as B
import System.Console.ANSI
import System.IO (localeEncoding)
import qualified Data.ByteString.Lazy as B
import Control.Exception (try, IOException)
import Control.Monad (when)

main :: IO ()
main = do
    putStrLn "Starting WeatherWander"
    args <- getArgs
    let location = getLocation args
    fetchDataAndProcess location

getLocation :: [String] -> String
getLocation args = case args of
    [] -> "London"  -- Default to London if no arguments are provided
    [loc] -> loc    -- Use the provided argument
    _ -> "Usage: WeatherWander <LOCATION>"

fetchDataAndProcess :: String -> IO ()
fetchDataAndProcess location = do
    result <- runExceptT $ fetchWeatherData location
    case result of
        Right response -> do
            writeFileResult <- try (B.writeFile "data/response.json" response) :: IO (Either IOException ())
            case writeFileResult of
                Right _ -> putStrLn "Weather data saved successfully."
                Left e -> putStrLn $ "Failed to write weather data to file: " ++ show e
        Left errorMsg -> putStrLn $ "Failed to fetch weather data: " ++ errorMsg

    parsingResult <- try parse :: IO (Either IOException (String, Float, String))
    case parsingResult of
        Right (place, temp, rain) -> do
            putStrLn $ "Checking places to visit in " ++ show place ++ " with temperature = " ++ show temp ++ ", Rain Status = " ++ show rain
            let city = place
                rainStatus = rain
                temperature = temp
            connectAndCreateTable city rainStatus temperature
            setSGR [SetConsoleIntensity BoldIntensity]
            putStrLn "\nHave a cracking time!"
            setSGR [Reset]
        Left e -> putStrLn $ "Failed to parse weather data: " ++ show e
