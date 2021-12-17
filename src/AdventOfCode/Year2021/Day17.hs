module AdventOfCode.Year2021.Day17
  ( run1,
    run2,
  )
where

import Data.Bifunctor (Bifunctor (bimap), second)
import Data.Ix (Ix (inRange))
import Data.List (unfoldr)
import Data.List.Extra (firstJust, maximumOn)
import Data.Maybe (fromMaybe)
import Data.Tuple.Extra (dupe)
import Debug.Trace (traceShow, traceShowId)
import Text.ParserCombinators.ReadP (string)
import Text.Read (lift, readPrec)

type Point = (Int, Int)

data TargetArea = TargetArea Point Point deriving (Show)

instance Read TargetArea where
  readPrec = do
    lift $ string "target area: x="
    a <- readPrec
    lift $ string ".."
    b <- readPrec
    lift $ string ", y="
    c <- readPrec
    lift $ string ".."
    d <- readPrec
    lift $ string "\n"
    return $ TargetArea (a, d) (b, c)

data Probe = Probe {position :: Point, velocity :: Point} deriving (Show)

nextStep :: Probe -> Probe
nextStep (Probe p v) =
  Probe
    (bimap ((+) $ fst v) ((+) $ snd v) p)
    (bimap (max 0 . (+ (-1))) (+ (-1)) v)

throw :: TargetArea -> Probe -> [Probe]
throw t = takeWhile (stop t) . iterate nextStep
  where
    stop (TargetArea (x1, y1) (x2, y2)) (Probe (x, y) _)
      | x > x2 = False
      | y < y2 = False
      | otherwise = True

isIn :: TargetArea -> Probe -> Bool
isIn (TargetArea (x1, y1) (x2, y2)) (Probe (x, y) _)
  | inRange (x1, x2) x && inRange (y2, y1) y = True
  | otherwise = False

process1 :: TargetArea -> Int
process1 t@(TargetArea (xMin, yMin) (xMax, yMax)) =
  maximum $
    fmap (snd . position) $
      snd $
        maximumOn (snd . fst) $
          filter
            (any (isIn t) . snd)
            $ fmap
              (second (throw t . Probe (0, 0)) . dupe)
              candidats
  where
    rangeX = [0 .. xMax]
    rangeY = [abs yMin .. abs yMax]
    candidats = concatMap ((`fmap` rangeY) . (,)) rangeX

run1 :: String -> String
run1 = show . process1 . read

process2 :: Int -> Int
process2 = id

run2 :: String -> String
run2 = const ""
