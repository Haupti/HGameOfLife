import Test.HUnit
import GridTest

gridUpdateTests :: Test
gridUpdateTests = TestList $ GridTest.testCases

main :: IO ()
main = do
  runTestTT gridUpdateTests
  return ()



