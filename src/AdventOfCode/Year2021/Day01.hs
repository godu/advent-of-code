module AdventOfCode.Year2021.Day01
  ( run1,
    process1,
    run2,
    process2,
  )
where

import Text.Read

process1 :: [Int] -> Int
process1 xs = length $ filter (uncurry (<)) $ zip xs (tail xs)

run1 :: String -> String
run1 = show . process1 . fmap read . lines

process2 :: Int -> Int
process2 = id

run2 :: String -> String
run2 = const ""
