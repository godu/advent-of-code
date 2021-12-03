module AdventOfCode.Year2021.Day03
  ( run1,
    process1,
    run2,
    process2,
  )
where

import AdventOfCode.Utils (read')
import Data.Bifunctor (bimap)
import Data.List (group, partition, transpose)
import Debug.Trace (traceShowId)
import Text.ParserCombinators.ReadP (skipSpaces, string)
import Text.ParserCombinators.ReadPrec ((+++))
import Text.Read (ReadPrec, lift, readPrec)

binaryToInt :: String -> Int
binaryToInt = foldl go 0
  where
    go n '1' = n * 2 + 1
    go n _ = n * 2

process1 :: [[Char]] -> Int
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

process2 :: Int -> Int
process2 = id

run2 :: String -> String
run2 = const ""
