
import Data.Either
import Data.Set (Set, fromList)
import Ananamer
import Test.HUnit
import Database.SQLite.Simple

main :: IO ()
main = runTestTT tests >>= (putStrLn . showCounts)

tests = TestList
  [ TestLabel "mergeStrings" mergeStringsTest
  , TestLabel "namegener works in simple sitution" namegenerMatchesExp
  , TestLabel "namegener follows anagram" namegenerSameCharacters 
  , TestLabel "namegener supports words that have spaces in them" namegenerStringsSupportsSpaces
  , TestLabel "Can load from database" testLoadNames
  , buildArgsYes
  ]

mergeStringsTest = TestCase (assertEqual "Merge string produces all possible merges" expected given)
  where expected = fromList ["Mark Markson", "Mark Johnson", "Mark Dinkson", "John Markson", "John Johnson", "John Dinkson", "Dink Markson", "Dink Johnson", "Dink Dinkson"]
        given = fromList $
          Ananamer.mergeStrings ["Mark", "John", "Dink"] ["Markson", "Johnson", "Dinkson"]

genericNamegener :: Switchs -> [String]
genericNamegener s = namegener fs ls s
  where fs  = ["John", "Mark", "Dink"]
        ls   = ["Johnson", "Markson", "Dinkson"]

defaultSwitch :: Switchs
defaultSwitch = Switchs{matchString = Nothing, random = False, help = False}

namegenerMatchesExp = TestCase (assertEqual "toy namegener call produces hand calculated result" expected given)
  where expected = fromList ["Mark Markson", "Mark Johnson", "Mark Dinkson", "John Markson", "John Johnson", "John Dinkson", "Dink Markson", "Dink Johnson", "Dink Dinkson"]
        given =
            fromList
          $ genericNamegener defaultSwitch

namegenerSameCharacters = TestCase (assertEqual "When given a name, namegener will provide anagram" expected given)
  where expected = ["Dink Dinkson"]
        given    = genericNamegener $
          defaultSwitch{matchString = Just "Dink Dinkson"}

namegenerStringsSupportsSpaces = TestCase (assertEqual "Merge string support names with spaces" expected given)
  where expected = ["San Bo Det"]
        given = namegener ["San Bo"] ["Det"] $
          defaultSwitch{matchString = Just "San Bo Det"}

testLoadNames = TestCase $ do
  let firsts = ["Mark", "John", "Dink"] :: [String]
  let lasts = fmap (\s -> s ++ "son") firsts
  let expected = tupleApply fromList (firsts, lasts)
  conn <- open ":memory:"
  execute conn "CREATE TABLE first_names (name text)" ()
  execute conn "CREATE TABLE last_names (name text)" ()
  executeMany conn "INSERT INTO first_names (name) VALUES (?)" $ fmap Only firsts
  executeMany conn "INSERT INTO last_names (name) VALUES (?)" $ fmap Only lasts
  calculated <- loadNames conn
  let calculated' = tupleApply fromList calculated
  close conn
  assertEqual "Capable of reading first & last names from sqlite database" expected calculated'

tupleApply :: (a -> b) -> (a, a) -> (b, b)
tupleApply f (x, y) = (f x, f y)

buildArgsYes = TestCase $ do
  -- This should probably use some kind of fuzzing to ensure it parses
  -- accurately.
  -- Also random will be added later
  let noArgs = []
  let argsName = ["name"]
  let argsRand = ["-r"]
  let argsRandLong = ["--random"]
  let argsRandName = ["-r", "name"]
  let argsDashName = ["--", "-name"]
  let argsFail = ["--drops"]
  let argsHelp = ["--help"]
  let argsHelpShort = ["-h"]

  let switchNoArgs = Right defaultSwitch
  let switchArgsName = Right defaultSwitch{matchString = Just "name"}
  let switchArgsRand = Right defaultSwitch{random = True}
  let switchArgsRandLong = switchArgsRand
  let switchArgsRandName = Right defaultSwitch{matchString = Just "name", random = True}
  let switchArgsDashName = Right defaultSwitch{matchString = Just "-name"}
  let switchArgsHelp = Right defaultSwitch{help = True}
  let switchArgsHelpShort = switchArgsHelp
  
  let calculatedNoArgs = processArgs noArgs
  let calculatedArgsName = processArgs argsName
  let calculatedArgsRand = processArgs argsRand
  let calculatedArgsRandLong = processArgs argsRandLong
  let calculatedArgsRandName = processArgs argsRandName
  let calculatedArgsDashName = processArgs argsDashName
  let calculatedArgsFail = processArgs argsFail
  let calculatedArgsHelp = processArgs argsHelp
  let calculatedArgsHelpShort = processArgs argsHelpShort
  assertEqual "Cannot handle no args" calculatedNoArgs switchNoArgs
  assertEqual "Cannot handle only name" calculatedArgsName switchArgsName
  assertEqual "Cannot handle only rand switch" switchArgsRand calculatedArgsRand
  assertEqual "Cannot handle only long rand switch" switchArgsRandLong calculatedArgsRandLong
  assertEqual "Cannot handle rand switch & name" switchArgsRandName calculatedArgsRandName
  assertEqual "Cannot handle dash-dash dash-name" switchArgsDashName calculatedArgsDashName
  assertBool "Invalid switch did not fail" $ isLeft calculatedArgsFail
  assertBool "Sets help flag if help switch is provided" $ help $ head $ rights [calculatedArgsHelp]
  assertBool "Sets help flag if (short) help switch is provided" $ help $ head $ rights [calculatedArgsHelpShort]

