module AdventOfCode.Day3
  ( run1,
    process1,
    run2,
    process2,
  )
where

import Data.List (unfoldr)

data Square = Tree | Open deriving (Show)

readSquare :: Char -> Square
readSquare '#' = Tree
readSquare _ = Open

readGrid :: String -> [[Square]]
readGrid = map (map readSquare) . lines

isTree :: Square -> Bool
isTree Tree = True
isTree _ = False

run1 :: String -> String
run1 = show . process1 1 3 . readGrid

process1 :: Int -> Int -> [[Square]] -> Int
process1 dx dy grid = sum $ unfoldr go (0, 0)
  where
    nbRow = length grid
    nbColumn = length $ head grid
    go :: (Int, Int) -> Maybe (Int, (Int, Int))
    go (x, y)
      | x >= nbRow = Nothing
      | otherwise =
        return
          ( if isTree $ grid !! x !! y then 1 else 0,
            (x + dx, (y + dy) `mod` nbColumn)
          )

run2 :: String -> String
run2 = show . process2 . readGrid

process2 :: [[Square]] -> Int
process2 grid =
  product $
    (\(dx, dy) -> process1 dx dy grid)
      <$> [(1, 1), (1, 3), (1, 5), (1, 7), (2, 1)]
