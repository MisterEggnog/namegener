
import Data.Set (Set, fromList)
import Ananamer
import Test.HUnit

main :: IO ()
main = runTestTT tests >>= (putStrLn . showCounts)

tests = TestList
  [ TestLabel "mergeStrings" mergeStringsTest
  , TestLabel "splitNames splits" splitNamesSplits
  , TestLabel "namegener works in simple sitution" namegenerMatchesExp
  , TestLabel "namegener follows anagram" namegenerSameCharacters 
  ]

mergeStringsTest = TestCase (assertEqual "Merge string produces all possible merges" expected given)
  where expected = fromList ["Mark Markson", "Mark Johnson", "Mark Dinkson", "John Markson", "John Johnson", "John Dinkson", "Dink Markson", "Dink Johnson", "Dink Dinkson"]
        given = fromList $
          Ananamer.mergeStrings ["Mark", "John", "Dink"] ["Markson", "Johnson", "Dinkson"]

splitNamesSplits = TestCase (assertEqual "splitNames splits according to newlines in string, no change in order" expected given)
  where expected = ["john", "mark", "dink"]
        given    = Ananamer.splitNameLists False "john\nmark\ndink\n"


genericNamegener :: Switchs -> [String]
genericNamegener s = namegener fs ls s
  where fs  = "John\nMark\nDink"
        ls   = "Johnson\nMarkson\nDinkson"

namegenerMatchesExp = TestCase (assertEqual "toy namegener call produces hand calculated result" expected given)
  where expected = fromList ["Mark Markson", "Mark Johnson", "Mark Dinkson", "John Markson", "John Johnson", "John Dinkson", "Dink Markson", "Dink Johnson", "Dink Dinkson"]
        given =
            fromList
          $ genericNamegener
          $ Switchs { matchString = Nothing, random = False }

namegenerSameCharacters = TestCase (assertEqual "When given a name, namegener will provide anagram" expected given)
  where expected = ["Dink Dinkson"]
        given    = genericNamegener $
          Switchs { matchString = Just "Dink Dinkson", random = False }
