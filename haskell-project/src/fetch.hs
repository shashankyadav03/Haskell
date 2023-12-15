{-# LANGUAGE OverloadedStrings #-}

module Fetch where

import Network.HTTP.Conduit (simpleHttp, HttpException)
import Data.ByteString.Lazy as B
import Data.Aeson (eitherDecode, FromJSON)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Except (runExceptT, ExceptT, throwError)
import Control.Exception (try)

key :: String
key = "ff3e5ba7f0131d75e2e2b74eead4f697"
fetchWeatherData :: String  -> ExceptT String IO B.ByteString
fetchWeatherData location = do
    let apiUrl = "http://api.weatherstack.com/current?access_key=" ++ key ++ "&query=" ++ location
    response <- liftIO $ try (simpleHttp apiUrl) :: ExceptT String IO (Either HttpException B.ByteString)
    case response of
        Right body -> return body
        Left err -> throwError $ "HTTP request failed: " ++ show err
