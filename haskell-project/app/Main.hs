--Main.hs
module Main(main, getLocation, fetchDataAndProcess) where

import Fetch (fetchWeatherData)
import Parse (parse)
import System.Environment (getArgs)
import Control.Monad.Except (runExceptT)
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.ByteString.Lazy as B
import Control.Exception (try, IOException)
-- import System.Console.ANSI

main :: IO ()
main = do
    putStrLn "\nStarting WeatherWander......."
    args <- getArgs
    let location = getLocation args
    fetchDataAndProcess location

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
            writeFileResult <- try (B.writeFile "data/response.json" response) :: IO (Either IOException ())
            case writeFileResult of
                Right _ -> putStrLn "\nWeather data saved successfully."
                Left e -> putStrLn $ "\nFailed to write weather data to file: " ++ show e
        Left errorMsg -> putStrLn $ "\nFailed to fetch weather data: " ++ errorMsg
    parse
