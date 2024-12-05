module AdventOfCode.Year2024.Day04
  ( run1,
    run2,
  )
where

import Data.List.Extra ((!?))
import Data.Maybe (catMaybes, mapMaybe)
import Debug.Trace (traceShowId)
import Prelude hiding (Word)

type WordSearch = [[Char]]

type Word = [Char]

type Position = (Int, Int)

allDirections (x, y) =
  [ [(x, y), (x - 1, y), (x - 2, y), (x - 3, y)],
    [(x, y), (x + 1, y), (x + 2, y), (x + 3, y)],
    [(x, y), (x, y - 1), (x, y - 2), (x, y - 3)],
    [(x, y), (x, y + 1), (x, y + 2), (x, y + 3)],
    [(x, y), (x - 1, y - 1), (x - 2, y - 2), (x - 3, y - 3)],
    [(x, y), (x - 1, y + 1), (x - 2, y + 2), (x - 3, y + 3)],
    [(x, y), (x + 1, y - 1), (x + 2, y - 2), (x + 3, y - 3)],
    [(x, y), (x + 1, y + 1), (x + 2, y + 2), (x + 3, y + 3)]
  ]

cross (x, y) =
  [ [ (x, y),
      (x - 1, y - 1),
      (x - 1, y + 1),
      (x + 1, y + 1),
      (x + 1, y - 1)
    ]
  ]

allPositions :: WordSearch -> [Position]
allPositions xs = (\(row, x) -> (\(col, y) -> (x, y)) <$> zip row [0 ..]) =<< zip xs [0 ..]

(!??) :: [[a]] -> (Int, Int) -> Maybe a
(!??) xs (x, y) = do
  row <- xs !? x
  row !? y

process :: WordSearch -> (WordSearch -> Position -> [Word]) -> (Word -> Bool) -> Int
process xs lookupCandidates isWinner = length $ winners
  where
    positions = allPositions xs
    candidates = concatMap (lookupCandidates xs) positions
    winners = filter isWinner candidates

process1 :: WordSearch -> Int
process1 xs = process xs lookupCandidates isWinner
  where
    lookupCandidates xs position = mapMaybe (xs !??) <$> allDirections position
    isWinner = (== "XMAS")

run1 :: String -> String
run1 = show . process1 . lines

process2 :: WordSearch -> Int
process2 xs = process xs lookupCandidates isWinner
  where
    lookupCandidates xs position = mapMaybe (xs !??) <$> cross position
    isWinner ['A', 'M', 'M', 'S', 'S'] = True
    isWinner ['A', 'M', 'S', 'S', 'M'] = True
    isWinner ['A', 'S', 'S', 'M', 'M'] = True
    isWinner ['A', 'S', 'M', 'M', 'S'] = True
    isWinner _ = False

run2 :: String -> String
run2 = show . process2 . lines
