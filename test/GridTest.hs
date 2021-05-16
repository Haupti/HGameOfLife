module GridTest where

import Test.HUnit
import Grid

singleCell :: [(Int,Int)]
singleCell = [(1,1)]

testCases :: [Test]
testCases = 
   cellNeighbourTestCases 
   ++ gridUpdateTestCases 
   ++ purgeCellsTestCases 
   ++ findNeighboursTestCases
   ++ findNewLivingCellsTestCases
   ++ createSurroundingTestCases

{-- Tests if cells are neighbours --}
cellNeighbourTestCases = [
    TestLabel "(1,1) is neighbour of (2,2)"
       (TestCase $ ((1,1) `isNeighbourOf` (2,2)) @?= True)
   ,TestLabel "(1,1) is not neighbour of (1,1)"
       (TestCase $ ((1,1) `isNeighbourOf` (1,1)) @?= False)
   ,TestLabel "(1,1) is not neighbour of (3,3)"
       (TestCase $ ((1,1) `isNeighbourOf` (3,3)) @?= False)
   ,TestLabel "(1,1) is neighbour of (0,1)" $
       (TestCase $ ((1,1) `isNeighbourOf` (0,1)) @?= True)
   ]

{-- Finding neighbours works correctly --}
findNeighboursTestCases = [
    TestLabel "No neighbours of single cell" 
      (TestCase $ (singleCell `findNeighboursOf` (1,1)) @?= [])
   ,TestLabel "one neighbours of two close cells" 
     (TestCase $ ([(3,2),(3,3)] `findNeighboursOf` (3,2)) @?= [(3,3)])
   ,TestLabel "n0 neighbours of two far apart cells" 
     (TestCase $ ([(5,5),(10,19)] `findNeighboursOf` (5,5)) @?= [])
   ,TestLabel "Non neighbours are sortetd out" 
      (TestCase $ ([(130,1),(3,4),(4,3),(5,2),(9,10),(131,0)] `findNeighboursOf` (4,3)) @?= [(3,4),(5,2)])
  ]

purgeCellsTestCases = [
   TestLabel "cell does not get removed from new grid" 
     (TestCase $ (purge [(1,1),(2,2),(3,3),(9,8)]) @?= [(2,2)])
   ,TestLabel "cell does get removed from new grid" 
     (TestCase $ (purge [(8,8),(8,9),(9,8)]) @?= [(8,8),(8,9),(9,8)])
   ]

{-- Create cell if coordinate has proper neighbourhood -}
findNewLivingCellsTestCases = [
      TestLabel "Finds cells in a neighbourhood of a grid that will be alive next evolution" $ TestCase $
         (findNewLivingCells [(1,1),(1,2),(1,3)]) @?=[(0,2),(2,2)]
   ]

{-- Create surrounding of a cell --}
createSurroundingTestCases = [
   TestLabel "Tests the creation of the surrounding of a cell" $ TestCase $
      (createSurrounding (3,3)) @?= [(2,2),(2,3),(2,4),(4,2),(4,3),(4,4),(3,2),(3,4)]
  ]

{-- Tests if the grid updates correctly --} 
gridUpdateTestCases = [
    TestLabel "Single cells with no neighbours dies"
       (TestCase $ (updateGrid singleCell) @?= [])
   ,TestLabel "Empty grid stays empty" $ 
       (TestCase $ (updateGrid []) @?= [])
   ,TestLabel "Block stays block" $ 
       (TestCase $ (updateGrid [(1,1),(1,2),(2,2),(2,1)]) @?= [(2,1),(2,2),(1,1),(1,2)])
   ,TestLabel "Blinker blinks" $ 
       (TestCase $ (
          let updatedGrid = updateGrid [(1,1),(1,2),(1,3)] in
            (0,2) `elem` updatedGrid
          &&(1,2) `elem` updatedGrid
          &&(2,2) `elem` updatedGrid
       ) @?= True)

   ]


