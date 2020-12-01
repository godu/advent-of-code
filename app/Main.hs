module Main
  ( main,
  )
where

import qualified AdventOfCode.Day1 as Day1 (run)
import Prelude

main :: IO ()
main = do
  print "Day1"
  fixture <- readFile "app/Inputs/Day1.txt"
  print $ "Result: " <> Day1.run fixture
