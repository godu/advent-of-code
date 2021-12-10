module AdventOfCode.Year2021.Day07
  ( run1,
    run2,
  )
where

import Data.Ix (range)
import Data.List.Extra (nub, sort, splitOn)

median :: [Int] -> Int
median x =
  if odd n
    then sort x !! (n `div` 2)
    else (sort x !! (n `div` 2 - 1) + sort x !! (n `div` 2)) `div` 2
  where
    n = length x

process :: (Int -> Int -> Int) -> [Int] -> Int
process fuel xs =
  minimum $
    (\x -> sum $ fuel x <$> xs)
      <$> range (minimum xs, maximum xs)

run1 :: String -> String
run1 = show . process fuel . fmap read . splitOn ","
  where
    fuel a b = abs (a - b)

run2 :: String -> String
run2 = show . process fuel . fmap read . splitOn ","
  where
    fuel a b = (n * (n + 1)) `div` 2
      where
        n = abs $ a - b
