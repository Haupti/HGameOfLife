module View where

import Grid
import Control.Concurrent

sampleGrid :: [(Int,Int)]
sampleGrid = [(2,2),(3,2),(4,2),(2,4),(3,4),(4,4),(4,3),(5,3)]

grid :: Int -> Int -> [[Char]]
grid x y = let row = ['_' | _<-[0..x]] 
           in [row | _<-[0..y]]

ranges :: [(Int,Int)] -> (Int,Int)
ranges grid = let firsts  = map (\(f,s) -> f) grid
                  seconds = map (\(fs,sn) -> sn) grid
              in 
                  (maximum firsts,maximum seconds) 

fillGrid :: [(Int,Int)] -> [[Char]]
fillGrid grd = insertCells grd $ grid (fst $ ranges grd) (snd $ ranges grd)

showGrid :: [[Char]] -> [IO ()]
showGrid grd | null grd = []
             | otherwise = (putStrLn $ show $ head grd):(showGrid $ tail grd)

asdf :: [(Int,Int)] -> IO ()
asdf cells = do  
    result <- sequence $ showGrid (fillGrid cells) 
    threadDelay (1000000)
    asdf (updateGrid cells)
    return ()
              

insertCells :: [(Int,Int)]->[[Char]]->[[Char]]
insertCells cells grid | null $ tail cells = insertCell (head cells) grid
                       | otherwise = insertCells (tail cells) (insertCell (head cells) grid)


insertCell :: (Int,Int) -> [[Char]] -> [[Char]]
insertCell (x,y) grid = 
    let ySplit = splitAt y grid in
    init (fst ySplit) ++ [insertCellInRow x (last (fst ySplit))] ++ snd ySplit 

insertCellInRow :: Int -> [Char] -> [Char]
insertCellInRow x row = let xSplit = splitAt x row in
                        init (fst xSplit) ++ ['O'] ++ snd xSplit
    





