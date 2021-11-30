module AdventOfCode.Year2020.Day10
  ( run1,
    process1,
    run2,
    process2,
  )
where

import Data.List (sort)
import Data.List.Split (splitOn)
import Data.Tuple (swap)

pairs :: [a] -> [(a, a)]
pairs [] = []
pairs [_] = []
pairs (x : x' : xs) = (x, x') : pairs (x' : xs)

process1 :: [Int] -> Int
process1 =
  uncurry (*)
    . foldl go (0, 1)
    . fmap (uncurry (-) . swap)
    . pairs
    . sort
    . (0 :)
  where
    go :: (Int, Int) -> Int -> (Int, Int)
    go (x, y) 1 = (x + 1, y)
    go (x, y) 3 = (x, y + 1)
    go x _ = x

run1 :: String -> String
run1 = show . process1 . fmap read . lines

arrangements :: [Int] -> Int
arrangements (x : y : xs) | x + y <= 3 = arrangements (y : xs) + arrangements (x + y : xs)
arrangements (_ : xs) = arrangements xs
arrangements _ = 1

process2 :: [Int] -> Int
process2 =
  product
    . fmap arrangements
    . splitOn [3]
    . fmap (uncurry (-) . swap)
    . pairs
    . sort
    . (0 :)

run2 :: String -> String
run2 = show . process2 . fmap read . lines
