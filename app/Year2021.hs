module Year2021
  ( main,
  )
where

import qualified AdventOfCode.Year2021.Day01 as Day01 (run1, run2)
import qualified AdventOfCode.Year2021.Day02 as Day02 (run1, run2)
import qualified AdventOfCode.Year2021.Day03 as Day03 (run1, run2)
import qualified AdventOfCode.Year2021.Day04 as Day04 (run1, run2)
import qualified AdventOfCode.Year2021.Day05 as Day05 (run1, run2)
import qualified AdventOfCode.Year2021.Day06 as Day06 (run1, run2)
import qualified AdventOfCode.Year2021.Day07 as Day07 (run1, run2)
import qualified AdventOfCode.Year2021.Day08 as Day08 (run1, run2)
import qualified AdventOfCode.Year2021.Day09 as Day09 (run1, run2)
import qualified AdventOfCode.Year2021.Day10 as Day10 (run1, run2)
import qualified AdventOfCode.Year2021.Day11 as Day11 (run1, run2)
import qualified AdventOfCode.Year2021.Day12 as Day12 (run1, run2)
import qualified AdventOfCode.Year2021.Day13 as Day13 (run1, run2)
import qualified AdventOfCode.Year2021.Day14 as Day14 (run1, run2)
import qualified AdventOfCode.Year2021.Day15 as Day15 (run1, run2)
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
import qualified AdventOfCode.Year2021.DayXX as DayXX (run1, run2)

main :: IO ()
main = do
  putStrLn "Day01"
  fixture <- readFile "app/Inputs/Year2021/Day01.txt"
  putStrLn $ "Result #1: " <> Day01.run1 fixture
  putStrLn $ "Result #2: " <> Day01.run2 fixture

  putStrLn "Day02"
  fixture <- readFile "app/Inputs/Year2021/Day02.txt"
  putStrLn $ "Result #1: " <> Day02.run1 fixture
  putStrLn $ "Result #2: " <> Day02.run2 fixture

  putStrLn "Day03"
  fixture <- readFile "app/Inputs/Year2021/Day03.txt"
  putStrLn $ "Result #1: " <> Day03.run1 fixture
  putStrLn $ "Result #2: " <> Day03.run2 fixture

  putStrLn "Day04"
  fixture <- readFile "app/Inputs/Year2021/Day04.txt"
  putStrLn $ "Result #1: " <> Day04.run1 fixture
  putStrLn $ "Result #2: " <> Day04.run2 fixture

  putStrLn "Day05"
  fixture <- readFile "app/Inputs/Year2021/Day05.txt"
  putStrLn $ "Result #1: " <> Day05.run1 fixture
  putStrLn $ "Result #2: " <> Day05.run2 fixture

  putStrLn "Day06"
  fixture <- readFile "app/Inputs/Year2021/Day06.txt"
  putStrLn $ "Result #1: " <> Day06.run1 fixture
  putStrLn $ "Result #2: " <> Day06.run2 fixture

  putStrLn "Day07"
  fixture <- readFile "app/Inputs/Year2021/Day07.txt"
  putStrLn $ "Result #1: " <> Day07.run1 fixture
  putStrLn $ "Result #2: " <> Day07.run2 fixture

  putStrLn "Day08"
  fixture <- readFile "app/Inputs/Year2021/Day08.txt"
  putStrLn $ "Result #1: " <> Day08.run1 fixture
  putStrLn $ "Result #2: " <> Day08.run2 fixture

  putStrLn "Day09"
  fixture <- readFile "app/Inputs/Year2021/Day09.txt"
  putStrLn $ "Result #1: " <> Day09.run1 fixture
  putStrLn $ "Result #2: " <> Day09.run2 fixture

  putStrLn "Day10"
  fixture <- readFile "app/Inputs/Year2021/Day10.txt"
  putStrLn $ "Result #1: " <> Day10.run1 fixture
  putStrLn $ "Result #2: " <> Day10.run2 fixture

  putStrLn "Day11"
  fixture <- readFile "app/Inputs/Year2021/Day11.txt"
  putStrLn $ "Result #1: " <> Day11.run1 fixture
  putStrLn $ "Result #2: " <> Day11.run2 fixture

  putStrLn "Day12"
  fixture <- readFile "app/Inputs/Year2021/Day12.txt"
  putStrLn $ "Result #1: " <> Day12.run1 fixture
  putStrLn $ "Result #2: " <> Day12.run2 fixture

  putStrLn "Day13"
  fixture <- readFile "app/Inputs/Year2021/Day13.txt"
  putStrLn $ "Result #1: " <> Day13.run1 fixture
  putStrLn $ "Result #2: \n" <> Day13.run2 fixture

  putStrLn "Day14"
  fixture <- readFile "app/Inputs/Year2021/Day14.txt"
  putStrLn $ "Result #1: " <> Day14.run1 fixture
  putStrLn $ "Result #2: " <> Day14.run2 fixture

  putStrLn "Day15"
  fixture <- readFile "app/Inputs/Year2021/Day15.txt"
  putStrLn $ "Result #1: " <> Day15.run1 fixture
  putStrLn $ "Result #2: " <> Day15.run2 fixture

  -- putStrLn "Day16"
  -- fixture <- readFile "app/Inputs/Year2021/Day16.txt"
  -- putStrLn $ "Result #1: " <> Day16.run1 fixture
  -- putStrLn $ "Result #2: " <> Day16.run2 fixture

  -- putStrLn "Day17"
  -- fixture <- readFile "app/Inputs/Year2021/Day17.txt"
  -- putStrLn $ "Result #1: " <> Day17.run1 fixture
  -- putStrLn $ "Result #2: " <> Day17.run2 fixture

  -- putStrLn "Day18"
  -- fixture <- readFile "app/Inputs/Year2021/Day18.txt"
  -- putStrLn $ "Result #1: " <> Day18.run1 fixture
  -- putStrLn $ "Result #2: " <> Day18.run2 fixture

  -- putStrLn "Day19"
  -- fixture <- readFile "app/Inputs/Year2021/Day19.txt"
  -- putStrLn $ "Result #1: " <> Day19.run1 fixture
  -- putStrLn $ "Result #2: " <> Day19.run2 fixture

  -- putStrLn "Day20"
  -- fixture <- readFile "app/Inputs/Year2021/Day20.txt"
  -- putStrLn $ "Result #1: " <> Day20.run1 fixture
  -- putStrLn $ "Result #2: " <> Day20.run2 fixture

  -- putStrLn "Day21"
  -- fixture <- readFile "app/Inputs/Year2021/Day21.txt"
  -- putStrLn $ "Result #1: " <> Day21.run1 fixture
  -- putStrLn $ "Result #2: " <> Day21.run2 fixture

  -- putStrLn "Day22"
  -- fixture <- readFile "app/Inputs/Year2021/Day22.txt"
  -- putStrLn $ "Result #1: " <> Day22.run1 fixture
  -- putStrLn $ "Result #2: " <> Day22.run2 fixture

  -- putStrLn "Day23"
  -- fixture <- readFile "app/Inputs/Year2021/Day23.txt"
  -- putStrLn $ "Result #1: " <> Day23.run1 fixture
  -- putStrLn $ "Result #2: " <> Day23.run2 fixture

  -- putStrLn "Day24"
  -- fixture <- readFile "app/Inputs/Year2021/Day24.txt"
  -- putStrLn $ "Result #1: " <> Day24.run1 fixture
  -- putStrLn $ "Result #2: " <> Day24.run2 fixture

  -- putStrLn "Day25"
  -- fixture <- readFile "app/Inputs/Year2021/Day25.txt"
  -- putStrLn $ "Result #1: " <> Day25.run1 fixture

  putStrLn "DayXX"
  fixture <- readFile "app/Inputs/Year2021/DayXX.txt"
  putStrLn $ "Result #1: " <> DayXX.run1 fixture
  putStrLn $ "Result #2: " <> DayXX.run2 fixture
