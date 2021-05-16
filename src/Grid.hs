module Grid where

import Data.List

{-- Evolves the input grid to the next time step using the game rules of game
  of life --}
updateGrid :: [(Int,Int)] -> [(Int,Int)]
updateGrid grid = 
  foldr (\cll grd -> insertCellIfProperNeighbourhood cll grd grid) (purge grid) (neighbourhood grid)

purge :: [(Int,Int)] -> [(Int,Int)]
purge grid = filter (\cell -> length (grid `findNeighboursOf` cell) `elem` [2,3]) grid

neighbourhood :: [(Int,Int)] -> [(Int,Int)]
neighbourhood = foldr (\cell acc -> (createSurrounding cell) ++ acc) [] 


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
  | otherwise = (xa <= xb+1 && xa >= xb-1) && (ya <= yb + 1 && ya >= yb -1)


{-- Create surrounding of a cell --}
createSurrounding :: (Int,Int) -> [(Int,Int)]
createSurrounding (x,y) = (x-1,y-1):(x-1,y):(x-1,y+1):(x+1,y-1):(x+1,y):(x+1,y+1):(x,y-1):(x,y+1):[] 

