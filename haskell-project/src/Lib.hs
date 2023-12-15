--Lib.hs
module Lib(getLocation) where

import Data.Char (toUpper, toLower)
import Data.List (intercalate)

-- Function get location
getLocation :: [String] -> String
getLocation args = case args of
    [] -> "London"  -- Default to London if no arguments are provided
    [loc] -> capitalizeWords loc   
    _ -> "\nUsage: WeatherWander <LOCATION>"

-- Function to capitalize each word in a string
capitalizeWords :: String -> String
capitalizeWords = intercalate " " . map capitalizeWord . words
  where
    capitalizeWord :: String -> String
    capitalizeWord [] = []
    capitalizeWord (x:xs) = toUpper x : map toLower xs



