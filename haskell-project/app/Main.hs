module Main where

import Fetch (fetchWeatherData)
import System.Environment (getArgs)
import Control.Monad.Except (runExceptT)
import qualified Data.ByteString.Lazy.Char8 as BC
import Lib
import Database
import Parse
-- import Cmm (CmmNode(args))


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
        (temp, rain) <- parse
        case (temp, rain) of
            (temp, rain) -> do
                putStrLn $ "Received values from parse: Temperature = " ++ show temp ++ ", Rain Status = " ++ show rain
            -- Continue with the rest of your main logic using temp and rain
            _ -> putStrLn "Failed to parse weather data"
        let city = "London"
            rainStatus = rain
            temperature = temp

        connectAndCreateTable city rainStatus temperature
        putStrLn "WeatherWander finished"
