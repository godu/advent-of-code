module AdventOfCode.Day6
  ( run1,
    run2,
    process2,
  )
where

import Data.List (nub)
import Data.List.Extra (splitOn)

run1 :: String -> String
run1 =
  show
    . sum
    . fmap
      ( length
          . nub
          . concat
          . lines
      )
    . splitOn "\n\n"

run2 :: String -> String
run2 = id

process2 :: Int -> Int
process2 = id
