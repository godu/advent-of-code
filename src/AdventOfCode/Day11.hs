{-# LANGUAGE TypeSynonymInstances #-}

module AdventOfCode.Day11
  ( run1,
    process1,
    run2,
    process2,
  )
where

import Data.List (unfoldr)
import Data.Maybe (mapMaybe)
import qualified Data.Vector as V (Vector, fromList, imap, toList, (!?))
import Debug.Trace
import Text.ParserCombinators.ReadP (string, (+++))
import Text.Read (Read (readPrec), lift)

data Cell
  = Empty
  | Occupied
  | Floor
  deriving (Show, Eq)

instance Read Cell where
  readPrec =
    lift $
      (Empty <$ string "L")
        +++ (Occupied <$ string "#")
        +++ (Floor <$ string ".")

type Grid a = V.Vector (V.Vector a)

(!?) :: Grid a -> (Int, Int) -> Maybe a
grid !? (x, y) = (V.!? y) =<< grid V.!? x

imap :: ((Int, Int) -> a -> b) -> Grid a -> Grid b
imap f = V.imap (\x -> V.imap (\y -> f (x, y)))

fold :: (a -> b -> b) -> b -> Grid a -> b
fold f = foldr (flip (foldr f))

singleton :: a -> [a]
singleton a = [a]

neighbors :: Grid a -> (Int, Int) -> [a]
neighbors grid (x, y) =
  mapMaybe
    (grid !?)
    [ (x -1, y -1),
      (x -1, y),
      (x -1, y + 1),
      (x, y -1),
      (x, y + 1),
      (x + 1, y -1),
      (x + 1, y),
      (x + 1, y + 1)
    ]

process1 :: Grid Cell -> Int
process1 =
  fold (\c -> if c == Occupied then (1 +) else id) 0
    . last
    . unfoldr go
  where
    go :: Grid Cell -> Maybe (Grid Cell, Grid Cell)
    go grid =
      if nextGrid == grid
        then Nothing
        else return (nextGrid, nextGrid)
      where
        nextGrid = nextGen grid

    nextGen :: Grid Cell -> Grid Cell
    nextGen grid = imap (computeCell grid) grid

    computeCell :: Grid Cell -> (Int, Int) -> Cell -> Cell
    computeCell grid i Empty =
      if (== 0) $ length $ filter (== Occupied) $ neighbors grid i
        then Occupied
        else Empty
    computeCell grid i Occupied =
      if (>= 4) $ length $ filter (== Occupied) $ neighbors grid i
        then Empty
        else Occupied
    computeCell _ _ x = x

readGrid :: Read a => String -> Grid a
readGrid =
  V.fromList
    . fmap
      ( V.fromList
          . fmap (read . singleton)
      )
    . lines

run1 :: String -> String
run1 = show . process1 . readGrid

process2 :: Int -> Int
process2 = id

run2 :: String -> String
run2 = id
