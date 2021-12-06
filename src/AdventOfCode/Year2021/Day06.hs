module AdventOfCode.Year2021.Day06
  ( run1,
    run2,
  )
where

import Data.Function (fix)
import Data.List.Split (splitOn)
import Debug.Trace (traceShow, traceShowId)

nTimes :: Int -> (a -> a) -> (a -> a)
nTimes 0 _ = id
nTimes 1 f = f
nTimes n f = f . nTimes (n -1) f

type Population = (Int, Int, Int, Int, Int, Int, Int, Int, Int)

population :: [Int] -> Population
population = fix go
  where
    append :: Population -> Population -> Population
    append (a, b, c, d, e, f, g, h, i) (a', b', c', d', e', f', g', h', i') =
      (a + a', b + b', c + c', d + d', e + e', f + f', g + g', h + h', i + i')
    go :: ([Int] -> Population) -> [Int] -> Population
    go rec (0 : xs) = append (0 + 1, 0, 0, 0, 0, 0, 0, 0, 0) $ rec xs
    go rec (1 : xs) = append (0, 0 + 1, 0, 0, 0, 0, 0, 0, 0) $ rec xs
    go rec (2 : xs) = append (0, 0, 0 + 1, 0, 0, 0, 0, 0, 0) $ rec xs
    go rec (3 : xs) = append (0, 0, 0, 0 + 1, 0, 0, 0, 0, 0) $ rec xs
    go rec (4 : xs) = append (0, 0, 0, 0, 0 + 1, 0, 0, 0, 0) $ rec xs
    go rec (5 : xs) = append (0, 0, 0, 0, 0, 0 + 1, 0, 0, 0) $ rec xs
    go rec (6 : xs) = append (0, 0, 0, 0, 0, 0, 0 + 1, 0, 0) $ rec xs
    go rec (7 : xs) = append (0, 0, 0, 0, 0, 0, 0, 0 + 1, 0) $ rec xs
    go rec (8 : xs) = append (0, 0, 0, 0, 0, 0, 0, 0, 0 + 1) $ rec xs
    go _ _ = (0, 0, 0, 0, 0, 0, 0, 0, 0)

count :: Population -> Int
count (a, b, c, d, e, f, g, h, i) = a + b + c + d + e + f + g + h + i

step :: Population -> Population
step (a, b, c, d, e, f, g, h, i) = (b, c, d, e, f, g, h + a, i, a)

process :: Int -> [Int] -> Int
process n = count . nTimes n step . population

run1 :: String -> String
run1 = show . process 80 . fmap read . splitOn ","

run2 :: String -> String
run2 = show . process 256 . fmap read . splitOn ","
