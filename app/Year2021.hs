module Year2021
  ( main,
  )
where

import qualified AdventOfCode.Year2021.Day01 as Day01 (run1, run2)

-- import qualified AdventOfCode.Year2021.Day02 as Day02 (run1, run2)
-- import qualified AdventOfCode.Year2021.Day03 as Day03 (run1, run2)
-- import qualified AdventOfCode.Year2021.Day04 as Day04 (run1, run2)
-- import qualified AdventOfCode.Year2021.Day05 as Day05 (run1, run2)
-- import qualified AdventOfCode.Year2021.Day06 as Day06 (run1, run2)
-- import qualified AdventOfCode.Year2021.Day07 as Day07 (run1, run2)
-- import qualified AdventOfCode.Year2021.Day08 as Day08 (run1, run2)
-- import qualified AdventOfCode.Year2021.Day09 as Day09 (run1, run2)
-- import qualified AdventOfCode.Year2021.Day10 as Day10 (run1, run2)
-- import qualified AdventOfCode.Year2021.Day11 as Day11 (run1, run2)
-- import qualified AdventOfCode.Year2021.Day12 as Day12 (run1, run2)
-- import qualified AdventOfCode.Year2021.Day13 as Day13 (run1, run2)
-- import qualified AdventOfCode.Year2021.Day14 as Day14 (run1, run2)
-- import qualified AdventOfCode.Year2021.Day15 as Day15 (run1, run2)
-- import qualified AdventOfCode.Year2021.Day16 as Day16 (run1, run2)
-- import qualified AdventOfCode.Year2021.Day17 as Day17 (run1, run2)
-- import qualified AdventOfCode.Year2021.Day18 as Day18 (run1, run2)
-- import qualified AdventOfCode.Year2021.Day19 as Day19 (run1, run2)
-- import qualified AdventOfCode.Year2021.Day20 as Day20 (run1, run2)
-- import qualified AdventOfCode.Year2021.Day21 as Day21 (run1, run2)
-- import qualified AdventOfCode.Year2021.Day22 as Day22 (run1, run2)
-- import qualified AdventOfCode.Year2021.Day23 as Day23 (run1, run2)
-- import qualified AdventOfCode.Year2021.Day24 as Day24 (run1, run2)
-- import qualified AdventOfCode.Year2021.Day25 as Day25 (run1)

main :: IO ()
main = do
  print "Day01"
  fixture <- readFile "app/Inputs/Year2021/Day01.txt"
  print $ "Result #1: " <> Day01.run1 fixture
  print $ "Result #2: " <> Day01.run2 fixture

-- print "Day02"
-- fixture <- readFile "app/Inputs/Year2021/Day02.txt"
-- print $ "Result #1: " <> Day02.run1 fixture
-- print $ "Result #2: " <> Day02.run2 fixture

-- print "Day03"
-- fixture <- readFile "app/Inputs/Year2021/Day03.txt"
-- print $ "Result #1: " <> Day03.run1 fixture
-- print $ "Result #2: " <> Day03.run2 fixture

-- print "Day04"
-- fixture <- readFile "app/Inputs/Year2021/Day04.txt"
-- print $ "Result #1: " <> Day04.run1 fixture
-- print $ "Result #2: " <> Day04.run2 fixture

-- print "Day05"
-- fixture <- readFile "app/Inputs/Year2021/Day05.txt"
-- print $ "Result #1: " <> Day05.run1 fixture
-- print $ "Result #2: " <> Day05.run2 fixture

-- print "Day06"
-- fixture <- readFile "app/Inputs/Year2021/Day06.txt"
-- print $ "Result #1: " <> Day06.run1 fixture
-- print $ "Result #2: " <> Day06.run2 fixture

-- print "Day07"
-- fixture <- readFile "app/Inputs/Year2021/Day07.txt"
-- print $ "Result #1: " <> Day07.run1 fixture
-- print $ "Result #2: " <> Day07.run2 fixture

-- print "Day08"
-- fixture <- readFile "app/Inputs/Year2021/Day08.txt"
-- print $ "Result #1: " <> Day08.run1 fixture
-- print $ "Result #2: " <> Day08.run2 fixture

-- print "Day09"
-- fixture <- readFile "app/Inputs/Year2021/Day09.txt"
-- print $ "Result #1: " <> Day09.run1 fixture
-- print $ "Result #2: " <> Day09.run2 fixture

-- print "Day10"
-- fixture <- readFile "app/Inputs/Year2021/Day10.txt"
-- print $ "Result #1: " <> Day10.run1 fixture
-- print $ "Result #2: " <> Day10.run2 fixture

-- print "Day11"
-- fixture <- readFile "app/Inputs/Year2021/Day11.txt"
-- print $ "Result #1: " <> Day11.run1 fixture
-- print $ "Result #2: " <> Day11.run2 fixture

-- print "Day12"
-- fixture <- readFile "app/Inputs/Year2021/Day12.txt"
-- print $ "Result #1: " <> Day12.run1 fixture
-- print $ "Result #2: " <> Day12.run2 fixture

-- print "Day13"
-- fixture <- readFile "app/Inputs/Year2021/Day13.txt"
-- print $ "Result #1: " <> Day13.run1 fixture
-- print $ "Result #2: " <> Day13.run2 fixture

-- print "Day14"
-- fixture <- readFile "app/Inputs/Year2021/Day14.txt"
-- print $ "Result #1: " <> Day14.run1 fixture
-- print $ "Result #2: " <> Day14.run2 fixture

-- print "Day15"
-- fixture <- readFile "app/Inputs/Year2021/Day15.txt"
-- print $ "Result #1: " <> Day15.run1 fixture
-- print $ "Result #2: " <> Day15.run2 fixture

-- print "Day16"
-- fixture <- readFile "app/Inputs/Year2021/Day16.txt"
-- print $ "Result #1: " <> Day16.run1 fixture
-- print $ "Result #2: " <> Day16.run2 fixture

-- print "Day17"
-- fixture <- readFile "app/Inputs/Year2021/Day17.txt"
-- print $ "Result #1: " <> Day17.run1 fixture
-- print $ "Result #2: " <> Day17.run2 fixture

-- print "Day18"
-- fixture <- readFile "app/Inputs/Year2021/Day18.txt"
-- print $ "Result #1: " <> Day18.run1 fixture
-- print $ "Result #2: " <> Day18.run2 fixture

-- print "Day19"
-- fixture <- readFile "app/Inputs/Year2021/Day19.txt"
-- print $ "Result #1: " <> Day19.run1 fixture
-- print $ "Result #2: " <> Day19.run2 fixture

-- print "Day20"
-- fixture <- readFile "app/Inputs/Year2021/Day20.txt"
-- print $ "Result #1: " <> Day20.run1 fixture
-- print $ "Result #2: " <> Day20.run2 fixture

-- print "Day21"
-- fixture <- readFile "app/Inputs/Year2021/Day21.txt"
-- print $ "Result #1: " <> Day21.run1 fixture
-- print $ "Result #2: " <> Day21.run2 fixture

-- print "Day22"
-- fixture <- readFile "app/Inputs/Year2021/Day22.txt"
-- print $ "Result #1: " <> Day22.run1 fixture
-- print $ "Result #2: " <> Day22.run2 fixture

-- print "Day23"
-- fixture <- readFile "app/Inputs/Year2021/Day23.txt"
-- print $ "Result #1: " <> Day23.run1 fixture
-- print $ "Result #2: " <> Day23.run2 fixture

-- print "Day24"
-- fixture <- readFile "app/Inputs/Year2021/Day24.txt"
-- print $ "Result #1: " <> Day24.run1 fixture
-- print $ "Result #2: " <> Day24.run2 fixture

-- print "Day25"
-- fixture <- readFile "app/Inputs/Year2021/Day25.txt"
-- print $ "Result #1: " <> Day25.run1 fixture
