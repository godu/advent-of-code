module Main
  ( main,
  )
where

import qualified AdventOfCode.Day1 as Day1 (run1, run2)
import Prelude

main :: IO ()
main = do
  print "Day1"
  fixture <- readFile "app/Inputs/Day1.txt"
  print $ "Result #1: " <> Day1.run1 fixture
  print $ "Result #2: " <> Day1.run2 fixture
