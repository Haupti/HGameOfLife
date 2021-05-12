module Lib where

import Grid
import View
import Control.Concurrent

someFunc = putStrLn "not implemented"

waitSecs :: Int -> IO ()
waitSecs n = threadDelay (n * 1000000)
