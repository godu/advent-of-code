module AdventOfCode.Year2021.Day22
  ( run1,
    run2,
  )
where

import Data.Foldable.Extra (sumOn')
import Data.Ix (inRange, rangeSize)
import Debug.Trace (traceShow, traceShowId)
import Text.ParserCombinators.ReadP (string)
import Text.Read (lift, readPrec, (+++), (<++))

data Cuboid = Cuboid {x :: (Int, Int), y :: (Int, Int), z :: (Int, Int)} deriving (Show, Eq)

instance Read Cuboid where
  readPrec = do
    lift $ string "x="
    xMin <- readPrec
    lift $ string ".."
    xMax <- readPrec
    lift $ string ",y="
    yMin <- readPrec
    lift $ string ".."
    yMax <- readPrec
    lift $ string ",z="
    zMin <- readPrec
    lift $ string ".."
    zMax <- readPrec
    return $ Cuboid (xMin, xMax) (yMin, yMax) (zMin, zMax)

count :: Cuboid -> Int
count (Cuboid x y z) = rangeSize x * rangeSize y * rangeSize z

data Step = Step {on :: Bool, cuboid :: Cuboid} deriving (Show, Eq)

instance Read Step where
  readPrec = do
    on <-
      (True <$ lift (string "on"))
        <++ (False <$ lift (string "off"))
    lift $ string " "
    Step on <$> readPrec

initialize :: [Step] -> Int
initialize = sum . fmap count . foldl go []
  where
    go :: [Cuboid] -> Step -> [Cuboid]
    go acc (Step True cuboid) = cuboid : (`substract` cuboid) `concatMap` acc
    go acc (Step False cuboid) = (`substract` cuboid) `concatMap` acc

    substract :: Cuboid -> Cuboid -> [Cuboid]
    a@(Cuboid (xMin, xMax) (yMin, yMax) (zMin, zMax)) `substract` b@(Cuboid (xMin', xMax') (yMin', yMax') (zMin', zMax'))
      | xMax' < xMin || xMax < xMin' = [a]
      | yMax' < yMin || yMax < yMin' = [a]
      | zMax' < zMin || zMax < zMin' = [a]
      | inRange (xMin, xMax) (xMin' - 1) =
        Cuboid (xMin, xMin' - 1) (yMin, yMax) (zMin, zMax) : Cuboid (xMin', xMax) (yMin, yMax) (zMin, zMax) `substract` b
      | inRange (xMin, xMax) (xMax' + 1) =
        Cuboid (xMax' + 1, xMax) (yMin, yMax) (zMin, zMax) : Cuboid (xMin, xMax') (yMin, yMax) (zMin, zMax) `substract` b
      | inRange (yMin, yMax) (yMin' - 1) =
        Cuboid (xMin, xMax) (yMin, yMin' - 1) (zMin, zMax) : Cuboid (xMin, xMax) (yMin', yMax) (zMin, zMax) `substract` b
      | inRange (yMin, yMax) (yMax' + 1) =
        Cuboid (xMin, xMax) (yMax' + 1, yMax) (zMin, zMax) : Cuboid (xMin, xMax) (yMin, yMax') (zMin, zMax) `substract` b
      | inRange (zMin, zMax) (zMin' - 1) =
        Cuboid (xMin, xMax) (yMin, yMax) (zMin, zMin' - 1) : Cuboid (xMin, xMax) (yMin, yMax) (zMin', zMax) `substract` b
      | inRange (zMin, zMax) (zMax' + 1) =
        Cuboid (xMin, xMax) (yMin, yMax) (zMax' + 1, zMax) : Cuboid (xMin, xMax) (yMin, yMax) (zMin, zMax') `substract` b
      | otherwise = []

process1 :: [Step] -> Int
process1 = initialize . (<> portions)
  where
    portions =
      [ Step False (Cuboid (minBound, -51) (minBound, maxBound) (minBound, maxBound)),
        Step False (Cuboid (51, maxBound) (minBound, maxBound) (minBound, maxBound)),
        Step False (Cuboid (minBound, maxBound) (51, maxBound) (minBound, maxBound)),
        Step False (Cuboid (minBound, maxBound) (minBound, -51) (minBound, maxBound)),
        Step False (Cuboid (minBound, maxBound) (minBound, maxBound) (51, maxBound)),
        Step False (Cuboid (minBound, maxBound) (minBound, maxBound) (minBound, -51))
      ]

run1 :: String -> String
run1 = show . process1 . fmap read . lines

process2 :: [Step] -> Int
process2 = initialize

run2 :: String -> String
run2 = show . process2 . fmap read . lines
