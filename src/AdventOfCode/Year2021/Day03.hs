module AdventOfCode.Year2021.Day03
  ( run1,
    process1,
    run2,
    process2,
  )
where

import Data.Bifunctor (bimap)
import Data.Function (on)
import Data.List (group, groupBy, partition, sort, sortBy, transpose, unfoldr)
import Data.List.Extra (maximumOn, minimumOn)
import GHC.Exts (groupWith)

binaryToInt :: String -> Int
binaryToInt = foldl go 0
  where
    go n '1' = n * 2 + 1
    go n _ = n * 2

process1 :: [String] -> Int
process1 =
  uncurry (*)
    . bimap binaryToInt binaryToInt
    . unzip
    . fmap (selectGammaAndEpsilon . partition (== '0'))
    . transpose
  where
    selectGammaAndEpsilon :: (String, String) -> (Char, Char)
    selectGammaAndEpsilon (zeros, ones)
      | length zeros > length ones = ('0', '1')
      | otherwise = ('1', '0')

run1 :: String -> String
run1 = show . process1 . lines

process2 :: [String] -> Int
process2 =
  uncurry (*)
    . bimap
      (binaryToInt . unfoldr leastCommon)
      (binaryToInt . unfoldr mostCommon)
    . (\x -> (x, x))
  where
    mostCommon :: [String] -> Maybe (Char, [String])
    mostCommon =
      extract
        . maximumOn length
        . sortBy (flip compare `on` head)
        . groupWith head
    leastCommon :: [String] -> Maybe (Char, [String])
    leastCommon =
      extract
        . minimumOn length
        . groupWith head
    extract :: [String] -> Maybe (Char, [String])
    extract xs@((x : _) : _) = return (x, tail <$> xs)
    extract _ = Nothing

run2 :: String -> String
run2 = show . process2 . lines
