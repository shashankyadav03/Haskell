module Main (main) where

import qualified Fetch (fetchWeatherData'')
-- import Lib
-- import Database (queryPlaces, Place)
-- import Parse (parseArgs)
import System.Environment (getArgs)

-- Main function, entry point of the application.
main :: IO ()
main = do
    putStrLn "Starting WeatherWander"
    args <- getArgs
    case args of
        [location] -> do
            weatherResult <- fetchWeatherData'' location
            putStrLn ("Usage: WeatherWander " ++ show weatherResult)
        [] -> putStrLn "Usage: WeatherWander <location>"
        _  -> putStrLn "Usage: WeatherWander <location>"
    -- case parseArgs args of
    --     Just city -> do
    --         weatherResult <- fetchWeatherData'' city
    --         putStrLn "Usage: WeatherWander" ++ show weatherResult
    --         case weatherResult of
    --             Right weatherData -> do
    --                 placesToVisit <- queryPlaces city weatherData
    --                 displayResults city placesToVisit
    --             Left error -> putStrLn $ "Error fetching weather data: " ++ error
    --     Nothing -> putStrLn "Usage: WeatherWander" ++ show args

-- Display the recommended places to visit.
-- displayResults :: String -> [City] -> IO ()
-- displayResults city places = do
--     putStrLn $ "Places to visit in " ++ city ++ ":"
--     mapM_ (putStrLn . show) places
