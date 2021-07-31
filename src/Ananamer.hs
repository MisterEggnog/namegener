module Ananamer where

import Data.Sort
import Data.List.Split
import Database.SQLite.Simple

main' :: Switchs -> IO ()
main' s = do
  conn <- open "names.db"
  (fns, lns) <- loadNames conn
  mapM_ putStrLn $ namegener fns lns s
  pure ()

-- From the passed in database, read the names from the `first_name` &
-- `last_name` tables.
-- The tuple returned is (first names, last names).
loadNames :: Connection -> IO ([String], [String])
loadNames db = do
  first_names <- query_ db "SELECT name FROM first_names;" :: IO [Only String]
  last_names <- query_ db "SELECT name FROM last_names;" :: IO [Only String]
  let first_names' = map fromOnly first_names
  let last_names' = map fromOnly last_names
  pure (first_names', last_names')

data Switchs = Switchs {
    matchString :: Maybe String -- This should have a space in it, this is not checked
  , random :: Bool -- Shuffle the first/last name lists, currently does nothing
  , help :: Bool -- Print help string
} deriving (Eq, Show)

-- From args list, process the args.
-- While I have looked into a commandline parser, the simple version was not
-- working & seemed like a lot of work for something that needs two args at
-- most.
processArgs :: [String] -> Either String Switchs
processArgs args = processArgs' args (Switchs{matchString = Nothing, random = False, help = False}) False
  where processArgs' :: [String] -> Switchs -> Bool -> Either String Switchs
        processArgs' [] s i = Right s
        processArgs' (x:xs) s i
          | x == "--help" || x == "-h" = Right s{help = True}
          | not i && (x == "-r" || x == "--random")
            = processArgs' xs (s{ random = True }) False
          | (head x /= '-') || i
            = processArgs' xs (s{ matchString = Just x}) i
          | x == "--" = processArgs' xs s True
          | otherwise = Left ("Uknown Switch: " ++ x)

helpStr :: String
helpStr = concat $ fmap (++"\n") [
  "Usage: ananamer [FLAGS] [name]",
  "Flags:",
  "    -r/--random: Attempt to shuffle the names before merging. Results may",
  "                 vary.",
  "    -h/--help: Print help message\n",
  "Args:",
  "    name: Name that the output should be an anagram of.",
  "Author: Baldwin, Josiah (2021)"]

-- First/Last names should be the file string, word splits will be added.
-- If switch.matchString is Just, then the returned list will only be those that have the same characters as matchString.
-- This is technically the default behaviour
namegener ::
  [String] ->  -- First names
  [String] ->  -- Last names
  Switchs -> -- Command switchs
  [String]
namegener fns lns sws =
  case matchString sws of
    Just s -> filter (\r -> prepS r == prepS s) names
    Nothing -> names
  where names   = mergeStrings fns lns
        prepS s = sort $ filter (/= ' ') s

splitNameLists ::
  Bool ->   -- If the returned string should be shuffled, current does nothing
  String -> -- The unsplit string
  [String]
splitNameLists r s = splitString
  where splitString = filter (/= "") $ splitOn "\n" s

-- first names -> last names
mergeStrings :: [String] -> [String] -> [String]
mergeStrings fns lns = nameM <$> fns <*> lns
  where nameM = \fn ln -> fn ++ " " ++ ln
