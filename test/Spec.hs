
import Data.Set (Set, fromList)
import Ananamer
import Test.HUnit

main :: IO ()
main = runTestTT tests >>= (putStrLn . showCounts)

tests = TestList [TestLabel "mergeStrings" mergeStringsTest]

mergeStringsTest = TestCase (assertEqual "Merge string produces all possible merges" expected given)
  where expected = fromList ["Mark Markson", "Mark Johnson", "Mark Dinkson", "John Markson", "John Johnson", "John Dinkson", "Dink Markson", "Dink Johnson", "Dink Dinkson"]
        given = fromList $
          Ananamer.mergeStrings ["Mark", "John", "Dink"] ["Markson", "Johnson", "Dinkson"]
