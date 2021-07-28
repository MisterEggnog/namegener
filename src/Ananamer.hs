module Ananamer where

import Data.Sort
import Data.List.Split
import Database.SQLite.Simple

main' :: IO ()
main' = undefined

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
}

-- First/Last names should be the file string, word splits will be added.
-- If switch.matchString is Just, then the returned list will only be those that have the same characters as matchString.
-- This is technically the default behaviour
namegener ::
  String ->  -- First names
  String ->  -- Last names
  Switchs -> -- Command switchs
  [String]
namegener fns lns sws =
  case matchString sws of
    Just s -> filter (\r -> prepS r == prepS s) names
    Nothing -> names
  where lns'    = splitNameLists (random sws) lns
        fns'    = splitNameLists (random sws) fns
        names   = mergeStrings fns' lns'
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
