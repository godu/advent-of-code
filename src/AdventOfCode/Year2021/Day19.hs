module AdventOfCode.Year2021.Day19
  ( run1,
    run2,
  )
where

import AdventOfCode.Utils (many, singleton, split)
import AdventOfCode.Year2021.Day18 (possibleTuples)
import Data.Function (on)
import Data.List (nub, sort)
import Data.Set as S (Set, fromList, intersection, map, toList, unions)
import Debug.Trace (traceShow, traceShowId)
import Text.ParserCombinators.ReadP (many1, string)
import Text.Read (Read (readPrec), get, lift)

data Point = Point Int Int Int deriving (Show, Eq, Ord)

instance Num Point where
  (Point x y z) + (Point x' y' z') = Point (x + x') (y + y') (z + z')
  (Point x y z) * (Point x' y' z') = Point (x * x') (y * y') (z * z')
  abs (Point x y z) = Point (abs x) (abs y) (abs z)
  fromInteger a = Point a' a' a' where a' = fromInteger a
  negate (Point x y z) = Point (-x) (-y) (-z)
  signum (Point x y z) = Point (signum x) (signum y) (signum z)

instance Read Point where
  readPrec = do
    x <- readPrec
    ',' <- get
    y <- readPrec
    ',' <- get
    Point x y <$> readPrec

data Scanner = Scanner {identifier :: Int, origin :: Point, points :: Set Point} deriving (Show, Eq)

translate :: (Point -> Point) -> Scanner -> Scanner
translate f (Scanner i p beacons) = Scanner i (f p) (S.map f beacons)

instance Read Scanner where
  readPrec = do
    lift $ string "--- scanner "
    i <- readPrec
    lift $ string " ---\n"
    Scanner i (Point 0 0 0) . fromList <$> split (lift (string "\n")) readPrec

newtype Input = Input [Scanner] deriving (Show, Eq)

instance Read Input where
  readPrec = Input <$> split (lift (string "\n\n")) readPrec

orientations :: [Point -> Point]
orientations =
  [ \(Point x y z) -> Point x y z,
    \(Point x y z) -> Point x (-z) y,
    \(Point x y z) -> Point x (-y) (-z),
    \(Point x y z) -> Point x z (-y),
    \(Point x y z) -> Point z y (-x),
    \(Point x y z) -> Point z x y,
    \(Point x y z) -> Point z (-y) x,
    \(Point x y z) -> Point z (-x) (-y),
    \(Point x y z) -> Point (-x) y (-z),
    \(Point x y z) -> Point (-x) z y,
    \(Point x y z) -> Point (-x) (-y) z,
    \(Point x y z) -> Point (-x) (-z) (-y),
    \(Point x y z) -> Point (-z) y x,
    \(Point x y z) -> Point (-z) (-x) y,
    \(Point x y z) -> Point (-z) (-y) (-x),
    \(Point x y z) -> Point (-z) x (-y),
    \(Point x y z) -> Point (-y) x z,
    \(Point x y z) -> Point (-y) (-z) x,
    \(Point x y z) -> Point (-y) (-x) (-z),
    \(Point x y z) -> Point (-y) z (-x),
    \(Point x y z) -> Point y (-x) z,
    \(Point x y z) -> Point y (-z) (-x),
    \(Point x y z) -> Point y x (-z),
    \(Point x y z) -> Point y z x
  ]

match :: [Scanner] -> Scanner -> [Scanner]
match ss s =
  concatMap (filter ((`any` ss) . hasTwelveBeaconInCommon) . (\s -> (`translate` s) <$> translations ss s)) orientedScanners
  where
    orientedScanners = fmap (`translate` s) orientations
    translations :: [Scanner] -> Scanner -> [Point -> Point]
    translations fs t = fmap (+) $ toList $ unions $ (\p -> S.map (p -) $ points t) <$> toList ps
      where
        ps = unions $ points <$> fs
    hasTwelveBeaconInCommon :: Scanner -> Scanner -> Bool
    hasTwelveBeaconInCommon a b =
      (>= 12) $
        length $
          points a `intersection` points b

recompose :: [Scanner] -> [Scanner]
recompose = head . go []
  where
    go :: [Scanner] -> [Scanner] -> [[Scanner]]
    go [] (x : xs) = go [x] xs
    go ss (x : xs) = case match ss x of
      [] -> go ss (xs <> [x])
      x -> concatMap ((`go` xs) . (ss <>) . singleton) x
    go ss [] = [ss]

process1 :: Input -> Int
process1 (Input scanners) = length $ toList $ unions $ points <$> recompose scanners

run1 :: String -> String
run1 = show . process1 . read

process2 :: Input -> Int
process2 (Input scanners) = maximum $ uncurry (manhattan `on` origin) <$> possibleTuples puzzle
  where
    puzzle = recompose scanners
    manhattan (Point x y z) (Point x' y' z') = sum $ abs <$> [x - x', y - y', z - z']

run2 :: String -> String
run2 = show . process2 . read
