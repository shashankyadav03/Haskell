module Main where

import Fetch (fetchWeatherData)
-- import Lib
-- import Database (queryPlaces, Place)
-- import Parse (parseArgs)
import System.Environment (getArgs)
import Control.Monad.Except (runExceptT)
import qualified Data.ByteString.Lazy.Char8 as BC


-- Main function, entry point of the application.
main :: IO ()
main = do
    putStrLn "Starting WeatherWander"
    args <- getArgs
    case args of
        [location] -> do
            result <- runExceptT $ fetchWeatherData location
            case result of
                Right response -> BC.putStrLn response
                Left errorMsg -> putStrLn $ "Failed to fetch weather data: " ++ errorMsg
        _ -> putStrLn "Usage: WeatherWander <LOCATION>"
    putStrLn "WeatherWander finished"