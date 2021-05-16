module GridTest where

import Test.HUnit
import Grid

singleCell :: [(Int,Int)]
singleCell = [(1,1)]

testCases :: [Test]
testCases = 
   cellNeighbourTestCases 
   ++ gridUpdateTestCases 
   ++ removeCellsTestCases 
   ++ findNeighboursTestCases
   ++ createCellTestCases
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

removeCellsTestCases = [
   TestLabel "cell does not get removed from new grid" 
     (TestCase $ (removeCellIfUnderOrOverpopulated (2,2) [(1,1),(2,2),(9,8)] [(1,1),(2,2),(3,3),(9,8)]) @?= [(1,1),(2,2),(9,8)])
   ,TestLabel "cell does get removed from new grid" 
     (TestCase $ (removeCellIfUnderOrOverpopulated (2,2) [(2,2),(9,8)] [(2,2),(8,19),(9,8)]) @?= [(9,8)])
   ]

{-- Create cell if coordinate has proper neighbourhood -}
createCellTestCases = [
    TestLabel "No cell is created" $ TestCase $ (insertCellIfProperNeighbourhood (3,1) [] [(3,2)]) @?= []
   ,TestLabel "No cell created because already in new" $ TestCase $ 
      let gridAtFirstStep = [(3,6),(4,6),(2,5),(3,7)]  
          actual = insertCellIfProperNeighbourhood (3,6) gridAtFirstStep gridAtFirstStep  
      in 
      actual @?= [(3,6),(4,6),(2,5),(3,7)]
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
       (TestCase $ (updateGrid [(1,1),(1,2),(2,2),(2,1)]) @?= [(1,1),(1,2),(2,2),(2,1)])
   ,TestLabel "Blinker blinks" $ 
       (TestCase $ (
          let updatedGrid = updateGrid [(1,1),(1,2),(1,3)] in
            (0,2) `elem` updatedGrid
          &&(1,2) `elem` updatedGrid
          &&(2,2) `elem` updatedGrid
       ) @?= True)

   ]


