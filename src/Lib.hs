module Lib where

import Grid
import Control.Concurrent

someFunc :: IO ()
someFunc = putStrLn "sasdfad"


waitSecs :: Int -> IO ()
waitSecs n = threadDelay (n * 1000000)
