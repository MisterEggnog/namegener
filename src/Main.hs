module Main where

main :: IO ()
main = undefined

newtype Switchs = Switchs {
   matchString :: Option String -- This should have a space in it, this is not checked
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
  case mathString sws of
    Just s -> undefined
    Nothing -> undefined
