{-# LANGUAGE TupleSections #-}

module AdventOfCode.Year2021.Day09
  ( run1,
    process1,
    run2,
    process2,
  )
where

import Data.Bifunctor (Bifunctor (bimap))
import Data.Char (digitToInt)
import Data.List (nub, sort, unfoldr)
import Data.Map (Map, delete, elems, filterWithKey, fromList, keys, member, toList, (!), (!?))
import Data.Maybe (mapMaybe)

type Point = (Int, Int)

type Height = Int

neighbors :: Point -> [Point]
neighbors (x, y) = [(x - 1, y), (x + 1, y), (x, y -1), (x, y + 1)]

lowers :: Map Point Height -> Map Point Height
lowers xs = filterWithKey (isLower xs) xs
  where
    isLower :: Map Point Height -> Point -> Height -> Bool
    isLower xs k x =
      all (> x) $
        mapMaybe (xs !?) $
          neighbors k

process1 :: Map Point Height -> Int
process1 = sum . fmap (+ 1) . lowers

run1 :: String -> String
run1 =
  show
    . process1
    . fromList
    . concatMap (\(x, line) -> bimap (x,) digitToInt <$> zip [0 ..] line)
    . zip [0 ..]
    . lines

spread :: Map Point Height -> Point -> Map Point Height
spread xs p = fromList $ unfoldr go ([p], xs)
  where
    go :: ([Point], Map Point Height) -> Maybe ((Point, Height), ([Point], Map Point Height))
    go ([], _) = Nothing
    go (p : ps, xs) = return (entry, (ns, p `delete` xs))
      where
        entry = (p, xs ! p)
        ns = filter (maybe False (< 9) . (xs !?)) $ nub $ ps ++ neighbors p

process2 :: Map Point Height -> Int
process2 xs =
  product $
    take 3 $
      reverse $
        sort $
          fmap (length . spread xs) $
            keys $
              lowers xs

run2 :: String -> String
run2 =
  show
    . process2
    . fromList
    . concatMap (\(x, line) -> bimap (x,) digitToInt <$> zip [0 ..] line)
    . zip [0 ..]
    . lines
