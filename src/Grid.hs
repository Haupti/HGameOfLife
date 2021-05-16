module Grid where

import Data.List

{-- Evolves the input grid to the next time step using the game rules of game
  of life --}
updateGrid :: [(Int,Int)] -> [(Int,Int)]
updateGrid grid = 
  let deathApplied = filter (\cell -> length (grid `findNeighboursOf` cell) `elem` [2,3]) grid
      neighbourhood = foldr (\cell acc -> (createSurrounding cell) ++ acc) [] grid
  in
  foldr (\neighbourhoodCell updatedGrid -> insertCellIfProperNeighbourhood neighbourhoodCell updatedGrid grid) deathApplied neighbourhood


{-- removes cell from new grid if underpopulated in old grid
  removeCellIfUnderOrOverpopulated cell newGrid oldGrid --}
removeCellIfUnderOrOverpopulated :: (Int,Int) -> [(Int,Int)] -> [(Int,Int)] -> [(Int,Int)]
removeCellIfUnderOrOverpopulated cell new old = if not (length (old `findNeighboursOf` cell) `elem` [2,3]) 
                                                 then new \\ [cell] 
                                                 else new


insertCellIfProperNeighbourhood :: (Int,Int) -> [(Int,Int)] -> [(Int,Int)] -> [(Int,Int)]
insertCellIfProperNeighbourhood cll new old = if length (old `findNeighboursOf` cll) == 3 
                                                 && (not $ cll `elem` new)
                                               then cll:new
                                               else new
                                               

findNeighboursOf :: [(Int,Int)] -> (Int,Int) -> [(Int,Int)]
findNeighboursOf grid p = filter (\gp -> gp `isNeighbourOf` p) grid


isNeighbourOf :: (Int,Int) -> (Int,Int) -> Bool
isNeighbourOf (xa,ya) (xb,yb)
  | (xa,ya) == (xb,yb) = False
  | otherwise = 
    if (xa <= xb+1 && xa >= xb-1)
    then if(ya <= yb + 1 && ya >= yb -1)
         then True
         else False
    else False

{-- Create surrounding of a cell --}
createSurrounding :: (Int,Int) -> [(Int,Int)]
createSurrounding (x,y) = (x-1,y-1):(x-1,y):(x-1,y+1):(x+1,y-1):(x+1,y):(x+1,y+1):(x,y-1):(x,y+1):[] 

