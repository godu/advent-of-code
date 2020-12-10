module AdventOfCode.Day10
  ( run1,
    process1,
    run2,
    process2,
  )
where

import Data.List (sort)

pairs :: [a] -> [(a, a)]
pairs [] = []
pairs [_] = []
pairs (x : x' : xs) = (x, x') : pairs (x' : xs)

process1 :: [Int] -> Int
process1 =
  uncurry (*)
    . foldl go (0, 1)
    . fmap (uncurry (-))
    . pairs
    . reverse
    . sort
    . (0 :)
  where
    go :: (Int, Int) -> Int -> (Int, Int)
    go (x, y) 1 = (x + 1, y)
    go (x, y) 3 = (x, y + 1)
    go x _ = x

run1 :: String -> String
run1 = show . process1 . fmap read . lines

process2 :: Int -> Int
process2 = id

run2 :: String -> String
run2 = id
