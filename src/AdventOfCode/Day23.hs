module AdventOfCode.Day23
  ( run1,
    process1,
    run2,
    process2,
  )
where

import AdventOfCode.Utils (singleton)
import Data.List (intercalate, partition)
import Data.List.Extra (trim)
import Data.Tuple (swap)

process1 :: Int -> [Int] -> [Int]
process1 = (answer .) . go
  where
    answer =
      tail
        . uncurry (<>)
        . swap
        . break (== 1)

    go :: Int -> [Int] -> [Int]
    go 0 xs = xs
    go move (x : y : y' : y'' : xs) =
      go
        (move - 1)
        (init <> [destinationCup] <> treeCups <> tail <> [currentCup])
      where
        currentCup = x
        treeCups = [y, y', y'']

        (init, destinationCup : tail) = break (== pred currentCup xs) xs

        pred :: Int -> [Int] -> Int
        pred x xs
          | (x - 1) `elem` xs = x - 1
          | x <= min = max
          | otherwise = pred (x - 1) xs
          where
            min = minimum xs
            max = maximum xs

run1 :: String -> String
run1 =
  intercalate ""
    . fmap show
    . process1 100
    . fmap (read . singleton)
    . trim

process2 :: Int -> Int
process2 = id

run2 :: String -> String
run2 = const ""
