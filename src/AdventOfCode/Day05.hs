module AdventOfCode.Day05
  ( run1,
    process1,
    run2,
  )
where

import Data.List (find)
import Data.Maybe (fromMaybe)

run1 :: String -> String
run1 =
  show
    . maximum
    . fmap process1
    . lines

process1 :: String -> Int
process1 code = row * 8 + column
  where
    (rowCode, columnCode) = splitAt 7 code

    row = fst $ foldl rowGo (0, 127) rowCode
    rowGo :: (Int, Int) -> Char -> (Int, Int)
    rowGo (f, b) 'F' = (f, b - ((b - f + 1) `div` 2))
    rowGo (f, b) _ = (f + (b - f + 1) `div` 2, b)

    column = fst $ foldl columnGo (0, 7) columnCode
    columnGo :: (Int, Int) -> Char -> (Int, Int)
    columnGo (l, r) 'L' = (l, r - ((r - l + 1) `div` 2))
    columnGo (l, r) _ = (l + ((r - l + 1) `div` 2), r)

run2 :: String -> String
run2 input =
  show $
    fromMaybe 0 $
      find
        ( \x ->
            x `notElem` seats
              && (x - 1) `elem` seats
              && (x + 1) `elem` seats
        )
        [0 .. (127 * 8 + 7)]
  where
    seats = process1 <$> lines input
