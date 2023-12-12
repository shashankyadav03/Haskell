module Main(main) where

import Fetch (fetchWeatherData)
import System.Environment (getArgs)
import Control.Monad.Except (runExceptT)
import qualified Data.ByteString.Lazy.Char8 as B
-- import Lib
import Database(connectAndCreateTable)
import Parse(parse)
-- import Cmm (CmmNode(args))
import System.Console.ANSI


-- Main function, entry point of the application.
main :: IO ()
main = do
        setSGR [SetConsoleIntensity BoldIntensity]
        putStrLn "\nStarting WeatherWander..........\n"
        setSGR [Reset]
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
            (placeResult, tempResult, rainResult) -> do
                putStrLn $ "Checking places to visit in " ++ show placeResult ++ " with temperature = " ++ show tempResult ++ ", Rain Status = " ++ show rainResult
            
        let city = place
            rainStatus = rain
            temperature = temp

        connectAndCreateTable city rainStatus temperature
        setSGR [SetConsoleIntensity BoldIntensity]
        putStrLn "\nHave a Great time!"
        setSGR [Reset]
