module View where

import Grid
import Control.Concurrent
import Data.List

sampleGrid :: [(Int,Int)]
sampleGrid = [(2,2),(3,2),(4,2),(2,4),(3,4),(4,4),(4,3),(5,3)]

-- drawPoints :: [(Int,Int)] -> [[Char]] -> [[Char]]

{-- draws a point in a grid. Does not draw point if outside grid --}
drawPoint :: (Int,Int) -> [[Char]] -> [[Char]] 
drawPoint (x,y) grd = replace y (replace x 'o' $ grd!!y) grd


{-- replace position newElement list returns a list where element at given
    position is replaced with new element --}
replace :: Int -> a -> [a] -> [a]
replace pos newElem list 
  | pos >= length list || pos < 0 = list
  | otherwise = let (left, right) = splitAt pos list in 
                left ++ [newElem] ++ (tail right)

{-- old --}

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
    





