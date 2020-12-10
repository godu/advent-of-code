module AdventOfCode.Day06
  ( run1,
    run2,
    process,
  )
where

import Data.List (intersect, nub, union)
import Data.List.Extra (splitOn)

run1 :: String -> String
run1 =
  show
    . process ((nub .) . union)

run2 :: String -> String
run2 =
  show
    . process intersect

process :: ([Char] -> [Char] -> [Char]) -> String -> Int
process append =
  sum
    . fmap
      ( length
          . foldl1 append
          . lines
      )
    . splitOn "\n\n"
