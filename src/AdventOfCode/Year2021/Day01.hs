module AdventOfCode.Year2021.Day01
  ( run1,
    process1,
    run2,
    process2,
  )
where

import Text.Read

pair :: [a] -> [(a, a)]
pair xs = zip xs (tail xs)

triplet :: [a] -> [(a, a, a)]
triplet xs = zip3 xs (tail xs) (tail $ tail xs)

process1 :: [Int] -> Int
process1 = length . filter (uncurry (<)) . pair

run1 :: String -> String
run1 = show . process1 . fmap read . lines

process2 :: [Int] -> Int
process2 = process1 . fmap sum . triplet
  where
    sum (a, b, c) = a + b + c

run2 :: String -> String
run2 = show . process2 . fmap read . lines
