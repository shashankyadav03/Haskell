module Main where

import Fetch (fetchWeatherData)
import System.Environment (getArgs)
import Control.Monad.Except (runExceptT)
import qualified Data.ByteString.Lazy.Char8 as B
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
                    Right response -> B.writeFile "data/response.json" response
                    Left errorMsg -> putStrLn $ "Failed to fetch weather data: " ++ errorMsg
            _ -> putStrLn "Usage: WeatherWander <LOCATION>"

        (place, temp, rain) <- parse
        case (place, temp, rain) of
            (place, temp, rain) -> do
                putStrLn $ "Checking places to visit in " ++ show place ++ " with temperature = " ++ show temp ++ ", Rain Status = " ++ show rain
            _ -> putStrLn "Failed to parse weather data"
        let city = place
            rainStatus = rain
            temperature = temp

        connectAndCreateTable city rainStatus temperature
        putStrLn "WeatherWander finished"
