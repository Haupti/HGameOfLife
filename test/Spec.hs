import Test.HUnit
import GridTest
import ViewTest


main :: IO ()
main = do
  runTestTT $ TestList $ GridTest.testCases 
  runTestTT $ TestList $ ViewTest.testCases 
  return ()



