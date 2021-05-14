module View where

import Grid
import Control.Concurrent

sampleGrid :: [(Int,Int)]
sampleGrid = [(2,2),(3,2),(4,2),(2,4),(3,4),(4,4),(4,3),(5,3)]


{-- genrates a grid with given with and height filed with ' ' chars --}
generateGrid :: Int -> Int -> [[Char]] 
generateGrid 0 _ = []
generateGrid w h = [[' ' | _ <- [1..w]] | _ <- [1..h]]


{-- draws a point in a grid. Does not draw point if outside grid --}
drawPoint :: (Int,Int) -> [[Char]] -> [[Char]] 
drawPoint (x,y) grd = replace y (replace x 'o' $ grd!!y) grd

{-- draws a 'o' at the given pos in the row. 
    Rows are treated as starting at 0 and only positive integers are allowed.
        --}
dotRow :: Int -> [Char] -> [Char]
dotRow x row = replace x 'o' row

{-- generatesDims generates dimensions of the minimum grid with the points from
    a list --}
generateDims :: [(Int,Int)] -> (Int,Int)
generateDims points = let xs = map (\p -> fst p) points
                          ys = map (\p -> snd p) points in
                      ((maximum xs - minimum xs),(maximum ys - minimum ys))

{-- replace position newElement list returns a list where element at given
    position is replaced with new element --}
replace :: Int -> a -> [a] -> [a]
replace pos newElem list 
  | pos >= length list || pos < 0 = list
  | otherwise = let (left, right) = splitAt pos list in 
                left ++ [newElem] ++ tail right














{-- old --}

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
    





