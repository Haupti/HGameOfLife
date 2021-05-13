module ViewTest where

import View
import Test.HUnit

testCases :: [Test]
testCases = testMarkerAtXPos 
            ++ testMarkerInRow
            ++ testGenerateGrid

{-- Tests whether inserting a marker at x pos in row works. This is normalized
  so the left most character is always at zero.--}
testMarkerAtXPos = [
    TestLabel "X at pos 2 in list" $ TestCase $ (dotRow 2 "   ") @?= "  o"
   ,TestLabel "X at pos 0 in list" $ TestCase $ (dotRow 0 "   ") @?= "o  "
   ,TestLabel "X at pos 10 in list" $ TestCase $ (dotRow 10 "012345678910") @?= "0123456789o0"
   ,TestLabel "X at pos 11 in list" $ TestCase $ (dotRow 11 "012345678910") @?= "01234567891o"
   ,TestLabel "X at pos 12 outside list" $ TestCase $ (dotRow 12 "012345678910") @?= "012345678910"
   ,TestLabel "X at pos -12 outside list" $ TestCase $ (dotRow (-12) "012345678910") @?= "012345678910"
  ]

{-- Test whether inserting marker in a grid uses the correct row --}

testMarkerInRow = [
    TestLabel "Y uses row 0 in grid" $ TestCase $ (drawPoint (0,0) ["  ","  "]) @?= ["o ","  "]
   ,TestLabel "Y uses row 1 in grid" $ TestCase $ (drawPoint (1,1) ["  ","  "]) @?= ["  "," o"]
   ,TestLabel "Y dont uses row 12 in grid" $ TestCase $ (drawPoint (1,12) ["  ","  "]) @?= ["  ","  "]
  ]

{-- create empty grid with given width and height --}
testGenerateGrid = [
    TestLabel "Generating a grid w=0,h=0 is empty" $ TestCase $ (generateGrid 0 0) @?= []
   ,TestLabel "Generating a grid w=1,h=1 works"    $ TestCase $ (generateGrid 1 1) @?= [" "]
   ,TestLabel "Generating a grid w=5,h=2 works"    $ TestCase $ (generateGrid 5 2) @?= ["     ","     "]
   ,TestLabel "Generating a grid w=2,h=4 works"    $ TestCase $ (generateGrid 2 4) @?= ["  ","  ","  ","  "]
   ]
