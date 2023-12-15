--Parse.hs
{-# LANGUAGE OverloadedStrings #-}
module Parse(getRainStatus, parseWeatherData, parse) where
-- Importing Modules
import GHC.Generics
import Data.Aeson
import qualified Data.ByteString.Lazy as BL 
import Database (connectDB, insertOrUpdateWeather, fetchWeatherInfo,connectAndCreateTable)
import Types(Request(..), Location(..), Current(..), WeatherData(..), WeatherRecord(..), WeatherInfo(..))
import Data.List (intercalate)

-- Defining Instances 
instance FromJSON Request where
    parseJSON = withObject "Request" $ \v -> Request
        <$> v .: "type"
        <*> v .: "query"
        <*> v .: "language"
        <*> v .: "unit"
instance FromJSON Location
instance FromJSON Current
instance FromJSON WeatherData

-- Determine rain status based on precipitation value
getRainStatus :: Float -> String
getRainStatus precipValue
    | precipValue < 0.4 = "Not Raining"
    | precipValue > 0.4 && precipValue < 0.6 = "May Rain"
    | otherwise = "Raining"

--Function for Parsing
parseWeatherData :: String -> IO (Maybe WeatherRecord)
parseWeatherData filePath = do
    putStrLn "Reading......"
    jsonContents <- BL.readFile filePath
    case eitherDecode jsonContents of
        Left err -> do
            putStrLn $ "Error decoding JSON: " ++ err
            return Nothing
            -- Handle the error, perhaps return a default WeatherRecord or exit
        Right weatherData -> do
            putStrLn "Reading Done!"
            let req = request weatherData
                loc = location weatherData
                cur = current weatherData
                place = name (location weatherData)
                record = WeatherRecord {
                        wrRequestType = requestType req,
                        wrQuery = query req,
                        wrLanguage = language req,
                        wrUnit = unit req,
                        wrLocationName = name loc,
                        wrCountry = country loc,
                        wrRegion = region loc,
                        wrLat = lat loc,
                        wrLon = lon loc,
                        wrTimezoneId = timezone_id loc,
                        wrLocaltime = localtime loc,
                        wrLocaltimeEpoch = localtime_epoch loc,
                        wrUtcOffset = utc_offset loc,
                        wrObservationTime = observation_time cur,
                        wrTemperature = temperature cur,
                        wrWeatherCode = weather_code cur,
                        wrWeatherIcons = intercalate ", " (weather_icons cur),  -- Convert list to string
                        wrWeatherDescriptions = intercalate ", " (weather_descriptions cur), -- Convert list to string
                        wrWindSpeed = wind_speed cur,
                        wrWindDegree = wind_degree cur,
                        wrWindDir = wind_dir cur,
                        wrPressure = pressure cur,
                        wrPrecip = precip cur,
                        wrHumidity = humidity cur,
                        wrCloudcover = cloudcover cur,
                        wrFeelslike = feelslike cur,
                        wrUvIndex = uv_index cur,
                        wrVisibility = visibility cur,
                        wrIsDay = is_day cur
                      }
            return (Just record)

--Main Parse Function
parse :: Int -> String -> IO ()
parse val place= do
    -- If value is 0, it uses json file saved from API call
    if val == 0 then 
        do 
            putStrLn "Fetching saved file"
            maybeRecord <- parseWeatherData "data/response.json"
            putStrLn "Conecting Database...."
            conn <- connectDB
            putStrLn "Connected!"
            case maybeRecord of
                Just record -> insertOrUpdateWeather conn record
                Nothing -> putStrLn "\nNo record to insert/update due to parsing error"
            maybeWeatherInfo <- fetchWeatherInfo conn place
            case maybeWeatherInfo of
                Just weatherInfo -> do
                    let precipitation = exprecipitation weatherInfo
                        temp_rature = extemperature weatherInfo
                        rainStatus = getRainStatus precipitation
                    putStrLn $ "\nChecking places to visit in " ++ show place ++ " with temperature = " ++ show temp_rature ++", Rain Status = " ++ show rainStatus
                    connectAndCreateTable conn place rainStatus temp_rature
                    putStrLn "\nHave a cracking time!"
                Nothing -> putStrLn "No data found for the specified location"
        -- Else extracts data from database as its withing 1 hour time frame
        else do
            putStrLn "Conecting Database...."
            conn <- connectDB
            maybeWeatherInfo <- fetchWeatherInfo conn place
            case maybeWeatherInfo of
                Just weatherInfo -> do
                    let precipitation = exprecipitation weatherInfo
                        temp_rature = extemperature weatherInfo
                        rainStatus = getRainStatus precipitation
                    putStrLn $ "\nChecking places to visit in " ++ show place ++ " with temperature = " ++ show temp_rature ++", Rain Status = " ++ show rainStatus
                    connectAndCreateTable conn place rainStatus temp_rature
                    putStrLn "\nHave a cracking time!"
                Nothing -> putStrLn "No data found for the specified location"
