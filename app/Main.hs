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
import qualified AdventOfCode.Day14 as Day14 (run1, run2)
import qualified AdventOfCode.Day15 as Day15 (run1, run2)
import qualified AdventOfCode.Day16 as Day16 (run1, run2)
import qualified AdventOfCode.Day17 as Day17 (run1, run2)
import qualified AdventOfCode.Day18 as Day18 (run1, run2)
import qualified AdventOfCode.Day19 as Day19 (run1, run2)
import qualified AdventOfCode.Day20 as Day20 (run1, run2)
import qualified AdventOfCode.Day21 as Day21 (run1, run2)

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
  print $ "Result #2: " <> Day13.run2 fixture

  print "Day14"
  fixture <- readFile "app/Inputs/Day14.txt"
  print $ "Result #1: " <> Day14.run1 fixture
  print $ "Result #2: " <> Day14.run2 fixture

  print "Day15"
  fixture <- readFile "app/Inputs/Day15.txt"
  print $ "Result #1: " <> Day15.run1 fixture
  print $ "Result #2: " <> Day15.run2 fixture

  print "Day16"
  fixture <- readFile "app/Inputs/Day16.txt"
  print $ "Result #1: " <> Day16.run1 fixture
  print $ "Result #2: " <> Day16.run2 fixture

  print "Day17"
  fixture <- readFile "app/Inputs/Day17.txt"
  print $ "Result #1: " <> Day17.run1 fixture
  print $ "Result #2: " <> Day17.run2 fixture

  print "Day18"
  fixture <- readFile "app/Inputs/Day18.txt"
  print $ "Result #1: " <> Day18.run1 fixture
  print $ "Result #2: " <> Day18.run2 fixture

  print "Day19"
  fixture <- readFile "app/Inputs/Day19.txt"
  print $ "Result #1: " <> Day19.run1 fixture
  print $ "Result #2: " <> Day19.run2 fixture

  print "Day20"
  fixture <- readFile "app/Inputs/Day20.txt"
  print $ "Result #1: " <> Day20.run1 fixture
  print $ "Result #2: " <> Day20.run2 fixture

  print "Day21"
  fixture <- readFile "app/Inputs/Day21.txt"
  print $ "Result #1: " <> Day21.run1 fixture
  print $ "Result #2: " <> Day21.run2 fixture
