module Main where

main :: IO ()
main = undefined

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
    Just s -> undefined
    Nothing -> undefined

splitNameLists ::
  String -> -- The unsplit string
  Bool ->   -- If the returned string should be shuffled, current does nothing
  [String]
splitNameLists s r = splitString
  where splitString = words s
