module Main (main) where

import Lib
import Database

main :: IO ()
--main = someFunc
main = do
  let city = "London"
      rain = "no"
      temp = 15

  connectAndCreateTable city rain temp