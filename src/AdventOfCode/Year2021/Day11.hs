{-# LANGUAGE TupleSections #-}

module AdventOfCode.Year2021.Day11
  ( run1,
    process1,
    run2,
    process2,
  )
where

import Data.Bifunctor (bimap)
import Data.Char (digitToInt)
import Data.Foldable (find)
import Data.List (findIndex, unfoldr)
import Data.Map as M (Map, elems, filter, fromList, keys, restrictKeys, union, unions, update)
import Data.Maybe (fromJust)
import Data.Set as S (Set, fromList, unions)

type Point = (Int, Int)

data State = Charging Int | Flashed deriving (Eq, Show)

type Cave = Map Point State

nTimes :: Int -> (c -> c) -> c -> c
nTimes 0 _ = id
nTimes 1 f = f
nTimes n f = f . nTimes (n -1) f

parse :: String -> Map Point State
parse =
  M.fromList
    . concatMap (\(x, line) -> bimap (x,) (Charging . digitToInt) <$> zip [0 ..] line)
    . zip [0 ..]
    . lines

generate :: Cave -> [Cave]
generate =
  unfoldr
    ( \cave ->
        let nextCave = nextStep cave
         in return
              ( nextCave,
                reset <$> nextCave
              )
    )
  where
    reset Flashed = Charging 0
    reset x = x
    willFlashed (Charging n) | n > 9 = True
    willFlashed _ = False
    incrementEnergy (Charging n) = Charging (n + 1)
    incrementEnergy x = x
    neighbors :: Point -> [Point]
    neighbors (x, y) = [(x - 1, y - 1), (x, y - 1), (x + 1, y - 1), (x - 1, y), (x + 1, y), (x - 1, y + 1), (x, y + 1), (x + 1, y + 1)]
    flash :: Cave -> Cave
    flash cave =
      (\cave -> foldr (update (return . incrementEnergy)) cave (concatMap neighbors flashed)) $
        (\cave -> foldr (update (const $ return Flashed)) cave flashed) $
          cave
      where
        flashed = keys $ M.filter willFlashed cave

    nextStep cave = fromJust $ find (not . any willFlashed . elems) $ iterate flash $ fmap incrementEnergy cave

process1 :: Cave -> Int
process1 =
  sum
    . take 100
    . fmap (length . M.filter (== Flashed))
    . generate

run1 :: String -> String
run1 = show . process1 . parse

process2 :: Cave -> Int
process2 =
  (+ 1)
    . fromJust
    . findIndex (all (== Flashed))
    . generate

run2 :: String -> String
run2 = show . process2 . parse
