{-# LANGUAGE TupleSections #-}

module AdventOfCode.Day24
  ( run1,
    process1,
    run2,
    process2,
  )
where

import Data.Ix (Ix (inRange))
import qualified Data.Map as M
import qualified Data.Set as S
import Text.ParserCombinators.ReadP (sepBy1, string)
import Text.Read (Read (readPrec), lift, minPrec, readListPrec, readPrec_to_P, (+++))

data Point = Point Int Int Int deriving (Show, Ord, Eq)

instance Semigroup Point where
  (<>) (Point x y z) (Point x' y' z') = Point (x + x') (y + y') (z + z')

instance Monoid Point where
  mempty = Point 0 0 0

data Direction = East | SouthEast | SouthWest | West | NorthWest | NorthEast deriving (Show)

instance Read Direction where
  readPrec =
    (East <$ lift (string "e"))
      +++ (SouthEast <$ lift (string "se"))
      +++ (SouthWest <$ lift (string "sw"))
      +++ (West <$ lift (string "w"))
      +++ (NorthWest <$ lift (string "nw"))
      +++ (NorthEast <$ lift (string "ne"))
  readListPrec =
    lift $ sepBy1 (readPrec_to_P readPrec minPrec) (string "")

directionToPoint :: Direction -> Point
directionToPoint East = Point 1 (-1) 0
directionToPoint SouthEast = Point 0 (-1) 1
directionToPoint SouthWest = Point (-1) 0 1
directionToPoint West = Point (-1) 1 0
directionToPoint NorthWest = Point 0 1 (-1)
directionToPoint NorthEast = Point 1 0 (-1)

generationGrid :: [[Direction]] -> S.Set Point
generationGrid =
  M.keysSet
    . M.filter ((== 1) . (`mod` 2))
    . M.fromListWith (+)
    . fmap
      ( (,1)
          . mconcat
          . fmap directionToPoint
      )

process1 :: [[Direction]] -> Int
process1 = length . generationGrid

run1 :: String -> String
run1 = show . process1 . fmap read . lines

neighbors :: Point -> S.Set Point
neighbors p =
  S.fromList $
    (<> p) . directionToPoint
      <$> [ East,
            SouthEast,
            SouthWest,
            West,
            NorthWest,
            NorthEast
          ]

process2 :: [[Direction]] -> Int
process2 = length . repeat go 100 . generationGrid
  where
    repeat f n x = foldr (const $ f) x [1 .. n]
    go :: S.Set Point -> S.Set Point
    go xs = S.filter (isAlive xs) xs'
      where
        xs' = S.unions $ neighbors <$> S.toList xs

        isAlive :: S.Set Point -> Point -> Bool
        isAlive xs x =
          if x `S.member` xs
            then inRange (1, 2) aliveNeighbors
            else aliveNeighbors == 2
          where
            aliveNeighbors = length $ xs `S.intersection` neighbors x

run2 :: String -> String
run2 = show . process2 . fmap read . lines
