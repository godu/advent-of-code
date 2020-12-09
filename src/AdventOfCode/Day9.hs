{-# LANGUAGE TupleSections #-}

module AdventOfCode.Day9
  ( run1,
    process1,
    run2,
    process2,
  )
where

import Data.List (find)
import Data.Maybe (fromMaybe)

pairs :: [a] -> [(a, a)]
pairs [] = []
pairs (x : xs) = ((x,) <$> xs) <> pairs xs

run1 :: String -> String
run1 = show . fromMaybe 0 . process1 25 . fmap read . lines

process1 :: Int -> [Int] -> Maybe Int
process1 i xs = uncurry go $ splitAt i xs
  where
    go _ [] = Nothing
    go xs (y : ys) =
      case find ((== y) . uncurry (+)) $ pairs $ take i xs of
        Nothing -> Just y
        Just _ -> go (tail xs <> [y]) ys

run2 :: String -> String
run2 = id

process2 :: Int -> Int
process2 = id
