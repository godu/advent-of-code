{-# LANGUAGE TupleSections #-}

module AdventOfCode.Day09
  ( run1,
    process1,
    run2,
    process2,
  )
where

import Data.List (find)
import Data.List.Split (divvy)
import Data.Maybe (fromMaybe)

pairs :: [a] -> [(a, a)]
pairs [] = []
pairs (x : xs) = ((x,) <$> xs) <> pairs xs

process :: Int -> [Int] -> Maybe (Int, [Int])
process i xs = uncurry go $ splitAt i xs
  where
    go _ [] = Nothing
    go xs (y : ys) =
      case find ((== y) . uncurry (+)) $ pairs $ take i xs of
        Nothing -> Just (y, xs)
        Just _ -> go (tail xs <> [y]) ys

process1 :: Int -> [Int] -> Maybe Int
process1 = (fmap fst .) . process

run1 :: String -> String
run1 = show . fromMaybe 0 . process1 25 . fmap read . lines

contiguous :: [a] -> [[a]]
contiguous xs = concatMap (\i -> divvy i 1 xs) [1 .. length xs]

process2 :: Int -> [Int] -> Maybe Int
process2 i xs =
  do
    (invalidNumber, _) <- process i xs
    cumputeWeakness
      <$> find
        ((== invalidNumber) . sum)
        (contiguous $ takeWhile (< invalidNumber) xs)
  where
    cumputeWeakness xs = maximum xs + minimum xs

run2 :: String -> String
run2 = show . fromMaybe 0 . process2 25 . fmap read . lines
