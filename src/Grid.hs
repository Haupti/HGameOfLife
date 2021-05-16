module Grid where

import Data.List

{-- Evolves the input grid to the next time step using the game rules of game
  of life --}
updateGrid :: [(Int,Int)] -> [(Int,Int)]
updateGrid grid = 
  let deathApplied = foldr (\cell newGrid -> 
        removeCellIfUnderOrOverpopulated cell newGrid grid) grid grid
      neighbourhood = foldr (\cell acc -> (createSurrounding cell) ++ acc) [] grid
  in
  foldr (\neighbourhoodCell updatedGrid -> insertCellIfProperNeighbourhood neighbourhoodCell updatedGrid grid) deathApplied neighbourhood
  
{-- removes cell from new grid if underpopulated in old grid
  removeCellIfUnderOrOverpopulated cell newGrid oldGrid
  --}
removeCellIfUnderOrOverpopulated :: (Int,Int) -> [(Int,Int)] -> [(Int,Int)] -> [(Int,Int)]
removeCellIfUnderOrOverpopulated cell new old = 
  let neighbourCount = length (old `findNeighboursOf` cell) 
  in
  if ( neighbourCount <= 1 || neighbourCount > 3)
  then new \\ [cell]
  else new


insertCellIfProperNeighbourhood :: (Int,Int) -> [(Int,Int)] -> [(Int,Int)] -> [(Int,Int)]
insertCellIfProperNeighbourhood cell new old = 
  let neighbourCount = length (old `findNeighboursOf` cell)
  in 
  if neighbourCount == 3 && not (cell `elem` new)
  then cell:new
  else new


findNeighboursOf :: [(Int,Int)] -> (Int,Int) -> [(Int,Int)]
findNeighboursOf grid p = filter (\gp -> gp `isNeighbourOf` p) grid

tupleEqual :: (Eq a, Eq b) => (a,b) -> (a,b) -> Bool
tupleEqual (xa,ya) (xb, yb) = (xa == xb) && (ya == yb)

isNeighbourOf :: (Int,Int) -> (Int,Int) -> Bool
isNeighbourOf (xa,ya) (xb,yb)
  | (xa == xb && ya == yb) = False
  | otherwise = 
    if (xa <= xb+1 && xa >= xb-1)
    then if(ya <= yb + 1 && ya >= yb -1)
         then True
         else False
    else False

{-- Create surrounding of a cell --}
createSurrounding :: (Int,Int) -> [(Int,Int)]
createSurrounding (x,y) = (x-1,y-1):(x-1,y):(x-1,y+1):(x+1,y-1):(x+1,y):(x+1,y+1):(x,y-1):(x,y+1):[] 

