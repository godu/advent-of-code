{-# LANGUAGE BangPatterns #-}

module AdventOfCode.Day15
  ( run1,
    run2,
  )
where

import Data.List (unfoldr)
import Data.List.Extra (splitOn)
import Data.Map (Map, empty, insert, (!?))

process :: [Int] -> [Int]
process xs =
  xs
    <> unfoldr
      go
      ( (1 + length xs, last xs),
        foldr (uncurry insert) empty $ zip xs $ fmap singleton [1 ..]
      )
  where
    go :: ((Int, Int), Map Int [Int]) -> Maybe (Int, ((Int, Int), Map Int [Int]))
    go ((i, v), !m) = case m !? v of
      Nothing -> return $ (0, ((i + 1, 0), update 0 i m))
      Just [x] -> return $ (0, ((i + 1, 0), update 0 i m))
      Just [x, y] -> return $ (x - y, ((i + 1, x - y), update (x - y) i m))

    update :: Int -> Int -> Map Int [Int] -> Map Int [Int]
    update i v m = case m !? i of
      Nothing -> insert i [v] m
      Just (x : _) -> insert i [v, x] m

singleton :: Int -> [Int]
singleton x = [x]

run1 :: String -> String
run1 = show . (!! (2020 - 1)) . process . fmap read . splitOn ","

run2 :: String -> String
run2 = show . (!! (30000000 - 1)) . process . fmap read . splitOn ","
