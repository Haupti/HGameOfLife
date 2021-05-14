module ViewTest where

import View 
import Test.HUnit

testCases :: [Test]
testCases = testMarkerInRow
            ++ testDrawPoints
            ++ testReNormalize
            ++ testAutoDrawPoints
            ++ testGenerateGrid
            ++ testGenerateGridDims

{-- Test whether inserting marker in a grid uses the correct row --}
testMarkerInRow = [
    TestLabel "Y uses row 0 in grid" $ TestCase $ (drawPoint (0,0) ["  ","  "]) @?= ["o ","  "]
   ,TestLabel "Y uses row 1 in grid" $ TestCase $ (drawPoint (1,1) ["  ","  "]) @?= ["  "," o"]
   ,TestLabel "Y dont uses row 12 in grid" $ TestCase $ (drawPoint (1,12) ["  ","  "]) @?= ["  ","  "]
  ]
testDrawPoints = [
    TestLabel "Drawing a set of points in grid" $ TestCase $ (drawOn [(1,1),(3,2),(2,2)] ["    ","    ","    "]) @?= ["    "," o  ","  oo"]
   ,TestLabel "Drawing a set of points in too small grid" $ TestCase $ (drawOn [(1,1),(3,2),(2,2)] ["   ","   "]) @?= ["   "," o "]
  ]

testReNormalize =
  [
    TestLabel "Renomalizes a single point to (0,0)" $ TestCase $ (renormalizePoints [(7,1)]) @?= [(0,0)]
   ,TestLabel "Renomalizes two point so the relative distance stays, but the there is point on x==0 and one on y==0" $ 
     TestList $ 
       [
        TestCase $ (renormalizePoints [(7,1),(8,2)]) @?= [(0,0),(1,1)]
       ,TestCase $ (renormalizePoints [(1,2),(2,1)]) @?= [(0,1),(1,0)]
       ]
  ]

testAutoDrawPoints =
  [
    TestLabel "Drawing a set of points on autogenerated grid" $ TestCase $ (autoDrawPoints [(1,1),(3,2),(2,2)]) @?= ["o  "," oo"]
   ,TestLabel "Drawing a set of points on autogenerated grid" $ TestCase $ (autoDrawPoints [(0,0),(2,1),(1,1)]) @?= ["o  "," oo"]
   ,TestLabel "Drawing a set of points on autogenerated grid" $ TestCase $ (autoDrawPoints [(6,1),(0,3),(1,1)]) @?= [" o    o","       ","o      "]
  ]


{-- create empty grid with given width and height --}
testGenerateGrid = [
    TestLabel "Generating a grid w=0,h=0 is empty" $ TestCase $ (generateGrid 0 0) @?= []
   ,TestLabel "Generating a grid w=0,h=1 is empty" $ TestCase $ (generateGrid 0 1) @?= []
   ,TestLabel "Generating a grid w=1,h=0 is empty" $ TestCase $ (generateGrid 1 0) @?= []
   ,TestLabel "Generating a grid w=1,h=1 works"    $ TestCase $ (generateGrid 1 1) @?= [" "]
   ,TestLabel "Generating a grid w=5,h=2 works"    $ TestCase $ (generateGrid 5 2) @?= ["     ","     "]
   ,TestLabel "Generating a grid w=2,h=4 works"    $ TestCase $ (generateGrid 2 4) @?= ["  ","  ","  ","  "]
   ]

testGenerateGridDims = 
   [ 
      TestLabel "Generating the dimensions of the grid from set of points" $ TestCase $ (autogenerateDimensions [(1,2),(5,3)]) @?= (5,2)
     ,TestLabel "Generating the dimensions of the grid from set of points" $ TestCase $ (autogenerateDimensions [(7,9),(7,9)]) @?= (1,1)
     ,TestLabel "Maximum is positive but minimun is negative"              $ TestCase $ (autogenerateDimensions [(1,2),(-5,-3)]) @?= (7,6)
     ,TestLabel "Maximum and minimum are negative"                         $ TestCase $ (autogenerateDimensions [(-1,-2),(-5,-3)]) @?= (5,2)
     ,TestLabel "Minimum and maximum are zero works"                       $ TestCase $ (autogenerateDimensions [(0,2),(-5,0)]) @?= (6,3)
   ]


