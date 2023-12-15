--Main.hs
module Main(main, getLocation) where
-- Importing Modules
import Fetch (fetchDataAndProcess)
import Parse (parse)
import Lib (getLocation)
import System.Environment (getArgs)
import Control.Monad.Except (runExceptT)
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.ByteString.Lazy as B
import Data.Time.Clock
import Data.Time.Format
import Database (connectDB, getDbTime)
import Control.Monad (join)


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
                    fetchResult <- fetchDataAndProcess location
                    case fetchResult of
                        Right _ -> parse 0 location -- proceed with parsing
                        Left errMsg -> putStrLn errMsg -- handle the error 
        Nothing -> do
            putStrLn "No data found for the specified location in our database"
            fetchResult <- fetchDataAndProcess location
            case fetchResult of
                Right _ -> parse 0 location -- proceed with parsing
                Left errMsg -> putStrLn errMsg -- handle the error
