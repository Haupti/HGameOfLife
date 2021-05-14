module ViewTest where

import View
import Test.HUnit

testCases :: [Test]
testCases = testMarkerInRow

{-- Test whether inserting marker in a grid uses the correct row --}
testMarkerInRow = [
    TestLabel "Y uses row 0 in grid" $ TestCase $ (drawPoint (0,0) ["  ","  "]) @?= ["o ","  "]
   ,TestLabel "Y uses row 1 in grid" $ TestCase $ (drawPoint (1,1) ["  ","  "]) @?= ["  "," o"]
   ,TestLabel "Y dont uses row 12 in grid" $ TestCase $ (drawPoint (1,12) ["  ","  "]) @?= ["  ","  "]
  ]
testDrawPoints = [
    TestLabel "Drawing a set of points without axis" $ TestCase $ "NA" @?= "NA"
  ]


