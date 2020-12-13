module Main
  ( main,
  )
where

import qualified AdventOfCode.Day01 as Day01 (run1, run2)
import qualified AdventOfCode.Day02 as Day02 (run1, run2)
import qualified AdventOfCode.Day03 as Day03 (run1, run2)
import qualified AdventOfCode.Day04 as Day04 (run1, run2)
import qualified AdventOfCode.Day05 as Day05 (run1, run2)
import qualified AdventOfCode.Day06 as Day06 (run1, run2)
import qualified AdventOfCode.Day07 as Day07 (run1, run2)
import qualified AdventOfCode.Day08 as Day08 (run1, run2)
import qualified AdventOfCode.Day09 as Day09 (run1, run2)
import qualified AdventOfCode.Day10 as Day10 (run1, run2)
import qualified AdventOfCode.Day11 as Day11 (run1, run2)
import qualified AdventOfCode.Day12 as Day12 (run1, run2)
import qualified AdventOfCode.Day13 as Day13 (run1, run2)
import Prelude

main :: IO ()
main = do
  print "Day01"
  fixture <- readFile "app/Inputs/Day01.txt"
  print $ "Result #1: " <> Day01.run1 fixture
  print $ "Result #2: " <> Day01.run2 fixture

  print "Day02"
  fixture <- readFile "app/Inputs/Day02.txt"
  print $ "Result #1: " <> Day02.run1 fixture
  print $ "Result #2: " <> Day02.run2 fixture

  print "Day03"
  fixture <- readFile "app/Inputs/Day03.txt"
  print $ "Result #1: " <> Day03.run1 fixture
  print $ "Result #2: " <> Day03.run2 fixture

  print "Day04"
  fixture <- readFile "app/Inputs/Day04.txt"
  print $ "Result #1: " <> Day04.run1 fixture
  print $ "Result #2: " <> Day04.run2 fixture

  print "Day05"
  fixture <- readFile "app/Inputs/Day05.txt"
  print $ "Result #1: " <> Day05.run1 fixture
  print $ "Result #2: " <> Day05.run2 fixture

  print "Day06"
  fixture <- readFile "app/Inputs/Day06.txt"
  print $ "Result #1: " <> Day06.run1 fixture
  print $ "Result #2: " <> Day06.run2 fixture

  print "Day07"
  fixture <- readFile "app/Inputs/Day07.txt"
  print $ "Result #1: " <> Day07.run1 fixture
  print $ "Result #2: " <> Day07.run2 fixture

  print "Day08"
  fixture <- readFile "app/Inputs/Day08.txt"
  print $ "Result #1: " <> Day08.run1 fixture
  print $ "Result #2: " <> Day08.run2 fixture

  print "Day09"
  fixture <- readFile "app/Inputs/Day09.txt"
  print $ "Result #1: " <> Day09.run1 fixture
  print $ "Result #2: " <> Day09.run2 fixture

  print "Day10"
  fixture <- readFile "app/Inputs/Day10.txt"
  print $ "Result #1: " <> Day10.run1 fixture
  print $ "Result #2: " <> Day10.run2 fixture

  print "Day11"
  fixture <- readFile "app/Inputs/Day11.txt"
  print $ "Result #1: " <> Day11.run1 fixture
  print $ "Result #2: " <> Day11.run2 fixture

  print "Day12"
  fixture <- readFile "app/Inputs/Day12.txt"
  print $ "Result #1: " <> Day12.run1 fixture
  print $ "Result #2: " <> Day12.run2 fixture

  print "Day13"
  fixture <- readFile "app/Inputs/Day13.txt"
  print $ "Result #1: " <> Day13.run1 fixture

-- print $ "Result #2: " <> Day13.run2 fixture
