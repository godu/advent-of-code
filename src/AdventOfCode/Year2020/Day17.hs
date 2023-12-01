{-# LANGUAGE FlexibleInstances #-}

module AdventOfCode.Year2020.Day17
  ( run1,
    run2,
  )
where

import AdventOfCode.Utils (singleton)
import Data.Ix (Ix (inRange))
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Set (Set, fromList, member, toList)
import Text.ParserCombinators.ReadP (string)
import Text.Read (Read (readPrec), lift, (+++))

data State = Active | Inactive deriving (Eq)

instance Show State where
  show Active = "#"
  show Inactive = "."

instance Read State where
  readPrec =
    (Active <$ lift (string "#"))
      +++ (Inactive <$ lift (string "."))

class Point a where
  range :: (a, a) -> [a]
  neighbors :: a -> [a]
  ounterBounds :: [a] -> (a, a)

instance Point (Int, Int, Int) where
  range ((x, y, z), (x', y', z')) =
    (\[x, y, z] -> (x, y, z))
      <$> sequence
        [ [x .. x'],
          [y .. y'],
          [z .. z']
        ]

  neighbors (x, y, z) =
    filter (/= (x, y, z)) $
      range
        ((x - 1, y - 1, z - 1), (x + 1, y + 1, z + 1))

  ounterBounds xs =
    ( add (-1) $ foldl (append min) (0, 0, 0) xs,
      add 1 $ foldl (append max) (0, 0, 0) xs
    )
    where
      append f (x, y, z) (x', y', z') = (f x x', f y y', f z z')
      add n (x, y, z) = (x + n, y + n, z + n)

newtype Grid a = Grid (Set a) deriving (Show, Eq)

generation :: (Point a, Ord a) => (State -> Int -> Bool) -> Grid a -> Grid a
generation willBeActive' (Grid grid) =
  Grid $
    fromList $
      filter willBeActive $
        range $
          ounterBounds $
            toList grid
  where
    willBeActive coord =
      willBeActive'
        (if coord `member` grid then Active else Inactive)
        $ length
        $ filter (`member` grid)
        $ neighbors coord

process1 :: Grid (Int, Int, Int) -> Int
process1 =
  length
    . toList
    . (\(Grid grid) -> grid)
    . generation willBeActive
    . generation willBeActive
    . generation willBeActive
    . generation willBeActive
    . generation willBeActive
    . generation willBeActive
  where
    willBeActive Active activeNeighbors = inRange (2, 3) activeNeighbors
    willBeActive Inactive activeNeighbors = activeNeighbors == 3

run1 :: String -> String
run1 =
  show
    . process1
    . Grid
    . fromList
    . fmap fst
    . filter ((== Active) . snd)
    . fmap (\(x, y, s) -> ((x, y, 0), s))
    . concatMap (\(x, xs) -> (\(y, s) -> (x, y, s)) <$> xs)
    . zip [0 ..]
    . fmap
      ( zip [0 ..]
          . fmap
            ( (read :: String -> State) . singleton
            )
      )
    . lines

instance Point (Int, Int, Int, Int) where
  range ((x, y, z, w), (x', y', z', w')) =
    (\[x, y, z, w] -> (x, y, z, w))
      <$> sequence
        [ [x .. x'],
          [y .. y'],
          [z .. z'],
          [w .. w']
        ]

  neighbors (x, y, z, w) =
    filter (/= (x, y, z, w)) $
      range
        ((x - 1, y - 1, z - 1, w - 1), (x + 1, y + 1, z + 1, w + 1))

  ounterBounds xs =
    ( add (-1) $ foldl (append min) (0, 0, 0, 0) xs,
      add 1 $ foldl (append max) (0, 0, 0, 0) xs
    )
    where
      append f (x, y, z, w) (x', y', z', w') = (f x x', f y y', f z z', f w w')
      add n (x, y, z, w) = (x + n, y + n, z + n, w + n)

process2 :: Grid (Int, Int, Int, Int) -> Int
process2 =
  length
    . toList
    . (\(Grid grid) -> grid)
    . generation willBeActive
    . generation willBeActive
    . generation willBeActive
    . generation willBeActive
    . generation willBeActive
    . generation willBeActive
  where
    willBeActive Active activeNeighbors = inRange (2, 3) activeNeighbors
    willBeActive Inactive activeNeighbors = activeNeighbors == 3

run2 :: String -> String
run2 =
  show
    . process2
    . Grid
    . fromList
    . fmap fst
    . filter ((== Active) . snd)
    . fmap (\(x, y, s) -> ((x, y, 0, 0), s))
    . concatMap (\(x, xs) -> (\(y, s) -> (x, y, s)) <$> xs)
    . zip [0 ..]
    . fmap
      ( zip [0 ..]
          . fmap
            ( (read :: String -> State) . singleton
            )
      )
    . lines
