--  namegener/ananamer - Program to generate actual name anagrams.
--  Copyright (C) 2021  Baldwin, Josiah
--
--  This program is free software: you can redistribute it and/or modify
--  it under the terms of the GNU General Public License as published by
--  the Free Software Foundation, either version 3 of the License, or
--  (at your option) any later version.
--
--  This program is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU General Public License for more details.
--
--  You should have received a copy of the GNU General Public License
--  along with this program.  If not, see <https://www.gnu.org/licenses/>.
module Ananamer where

import Data.Sort
import Database.SQLite.Simple
import Data.Text (pack)

main' :: Switchs -> IO ()
main' s = do
  conn <- open "names.db"
  (fns, lns) <- loadNames conn (random s)
  mapM_ putStrLn $ namegener fns lns s
  pure ()

-- From the passed in database, read the names from the `first_name` &
-- `last_name` tables.
-- The tuple returned is (first names, last names).
-- If the passed in Bool is True, than a query that attempts to randomize
-- the output is used.
loadNames :: Connection -> Bool -> IO ([String], [String])
loadNames db r = do
  first_names <- query_ db firstNameQuery :: IO [Only String]
  last_names <- query_ db lastNameQuery :: IO [Only String]
  let first_names' = map fromOnly first_names
  let last_names' = map fromOnly last_names
  pure (first_names', last_names')
  where firstNameQuery = nameQuery "first"
        lastNameQuery = nameQuery "last"
        nameQuery = \s -> Query $ pack $ if r then
            randNameQuery s
          else
            staticNameQuery s
        staticNameQuery = \s -> "SELECT DISTINCT name FROM " ++ s ++ "_names;"
        randNameQuery = \s -> "SELECT name FROM " ++ s ++ "_names ORDER BY RANDOM();"

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
helpStr = unlines [
  "Usage: ananamer [FLAGS] [name]",
  "Flags:",
  "    -r/--random: Attempt to shuffle the names before merging. Results may",
  "                 vary.",
  "    -h/--help: Print help message\n",
  "Args:",
  "    name: Name that the output should be an anagram of.",
  "",
  "Copyright (C) 2021  Baldwin, Josiah",
  "This program comes with ABSOLUTELY NO WARRANTY.",
  "This is free software, and you are welcome to redistribute it under certain",
  "conditions; see <https://www.gnu.org/licenses/> for more information."]

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

-- first names -> last names
mergeStrings :: [String] -> [String] -> [String]
mergeStrings fns lns = nameM <$> fns <*> lns
  where nameM = \fn ln -> fn ++ " " ++ ln
