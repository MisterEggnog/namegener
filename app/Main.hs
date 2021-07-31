module Main where

import Ananamer
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  let switchs = Ananamer.processArgs args
  case switchs of
    Left err -> putStrLn err
    Right sws ->
      if help sws then
        putStr Ananamer.helpStr
      else
        main' sws

