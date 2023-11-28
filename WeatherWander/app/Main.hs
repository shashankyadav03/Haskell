module Main where

import Fetch (fetchWeatherData, WeatherData)
import Database (queryPlaces, Place)
import Parse (parseArgs)
import System.Environment (getArgs)

-- Main function, entry point of the application.
main :: IO ()
main = do
    args <- getArgs
    case parseArgs args of
        Just city -> do
            weatherResult <- fetchWeatherData city
            case weatherResult of
                Right weatherData -> do
                    placesToVisit <- queryPlaces city weatherData
                    displayResults city placesToVisit
                Left error -> putStrLn $ "Error fetching weather data: " ++ error
        Nothing -> putStrLn "Usage: WeatherWander <city>"

-- Display the recommended places to visit.
displayResults :: String -> [Place] -> IO ()
displayResults city places = do
    putStrLn $ "Places to visit in " ++ city ++ ":"
    mapM_ (putStrLn . show) places
