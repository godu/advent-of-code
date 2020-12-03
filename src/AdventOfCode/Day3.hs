module AdventOfCode.Day3
  ( run1,
    process1,
    run2,
    process2,
  )
where

import Text.Read (Read (readPrec))

data Square = Tree | Open deriving (Show)

readSquare :: Char -> Square
readSquare '#' = Tree
readSquare _ = Open

run1 :: String -> String
run1 = show . process1 . map (map readSquare) . lines

process1 :: [[Square]] -> Int
process1 grid = go grid (0, 0) 0
  where
    nbRow = length grid
    nbColumn = length $ head grid
    go :: [[Square]] -> (Int, Int) -> Int -> Int
    go _ (x, _) trees | x == nbRow = trees
    go grid (x, y) trees =
      go
        grid
        (x + 1, (y + 3) `mod` nbColumn)
        nextTrees
      where
        square = grid !! x !! y
        nextTrees = case square of
          Tree -> trees + 1
          Open -> trees

run2 :: String -> String
-- run2 = show . read
run2 = id

process2 :: Int -> Int
process2 = id
