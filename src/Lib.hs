module Lib where

import Control.Concurrent

{-- some samples --}
blinker :: [(Int,Int)]
blinker   = [(1,1),(2,1),(3,1)]
glider :: [(Int,Int)]
glider    = [(0,1),(1,2),(2,0),(2,1),(2,2)]
demo :: [(Int,Int)]
demo      = [(0,0),(0,1),(0,2),(1,0),(1,2),(2,0),(2,2),(4,0),(4,2),(5,0),(5,2),(6,0),(6,1),(6,2)]

waitSecs :: Int -> IO ()
waitSecs n = threadDelay (n * 1000000)
