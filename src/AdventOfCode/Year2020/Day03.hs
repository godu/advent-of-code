module AdventOfCode.Year2020.Day03
  ( run1,
    process1,
    run2,
    process2,
  )
where

import Data.List (unfoldr)

data Square = Tree | Open deriving (Eq)

readSquare :: Char -> Square
readSquare '#' = Tree
readSquare _ = Open

readGrid :: String -> [[Square]]
readGrid = map (map readSquare) . lines

run1 :: String -> String
run1 = show . process1 1 3 . readGrid

process1 :: Int -> Int -> [[Square]] -> Int
process1 dx dy grid = length $ filter (== Tree) $ unfoldr go (0, 0)
  where
    nbRow = length grid
    nbColumn = length $ head grid
    go :: (Int, Int) -> Maybe (Square, (Int, Int))
    go (x, y)
      | x >= nbRow = Nothing
      | otherwise =
          return
            ( grid !! x !! y,
              (x + dx, (y + dy) `mod` nbColumn)
            )

run2 :: String -> String
run2 = show . process2 . readGrid

process2 :: [[Square]] -> Int
process2 grid =
  product $
    (\(dx, dy) -> process1 dx dy grid)
      <$> [(1, 1), (1, 3), (1, 5), (1, 7), (2, 1)]
