module Main where

import Lib
import Grid
import View

main :: IO ()
main = main' demo 

main' :: [(Int,Int)] -> IO ()
main' grd = do
  sequence $ (showGrid $ drawPoints (updateGrid grd) (0,15) (0,15)) ++[putStrLn "___________________________"]
  waitSecs 1
  main' $ updateGrid grd
  return ()

showGrid :: (Show a)=> [a] -> [IO ()]
showGrid [] = []
showGrid (g:gs) = (putStrLn $ show g):(showGrid gs)
