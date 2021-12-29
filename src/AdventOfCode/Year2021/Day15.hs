module AdventOfCode.Year2021.Day15
  ( run1,
    run2,
  )
where

import AdventOfCode.Utils (nTimes)
import Algorithm.Search (aStar, dijkstra)
import Data.Bifunctor (bimap, first, second)
import Data.Char (digitToInt, intToDigit)
import Data.Function (on)
import Data.Ix (range)
import Data.List as L (insert, insertBy)
import Data.List.Extra (firstJust, nubOrdOn, sortOn)
import Data.Map as M (Map, empty, foldrWithKey, fromList, insert, keys, mapKeys, member, unions, (!), (!?))
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Set as S (Set, delete, empty, insert, member)
import Data.Tuple (swap)
import Debug.Trace (traceShow, traceShowId)

type Point = (Int, Int)

parse :: String -> Map Point Int
parse =
  fromList
    . concatMap
      ( uncurry
          ( (. zip [0 ..])
              . fmap
              . (`bimap` digitToInt)
              . (swap .)
              . (,)
          )
      )
    . zip [0 ..]
    . lines

neighbors :: Map Point Int -> Point -> [Point]
neighbors m (x, y) = filter (`M.member` m) [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]

process1 :: Map Point Int -> Int
process1 m = maybe 0 fst $ aStar (neighbors m) cost (estimate final) (== final) initial
  where
    cost _ p = fromMaybe maxBound $ m !? p
    estimate (x, y) (x', y') = (x - x') + (y - y')
    initial = minimum $ keys m
    final = maximum $ keys m

run1 :: String -> String
run1 = show . process1 . parse

increment :: Int -> Int
increment 9 = 1
increment n = n + 1

process2 :: String -> String
process2 = unlines . fmap (fmap intToDigit . duplicateColumn) . duplicateRow . fmap (fmap digitToInt) . lines
  where
    duplicateColumn :: [Int] -> [Int]
    duplicateColumn xs = concatMap (\i -> nTimes i increment <$> xs) [0 .. 4]
    duplicateRow :: [[Int]] -> [[Int]]
    duplicateRow xs = concatMap (\i -> fmap (nTimes i increment) <$> xs) [0 .. 4]

run2 :: String -> String
run2 = show . process1 . parse . process2
