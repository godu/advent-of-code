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

data TargetArea = TargetArea Int Int Int Int deriving (Show)

instance Read TargetArea where
  readPrec = do
    lift $ string "target area: x="
    left <- readPrec
    lift $ string ".."
    right <- readPrec
    lift $ string ", y="
    bottom <- readPrec
    lift $ string ".."
    top <- readPrec
    lift $ string "\n"
    return $ TargetArea left right bottom top

data Probe = Probe {position :: Point, velocity :: Point} deriving (Show)

nextStep :: Probe -> Probe
nextStep (Probe p v) =
  Probe
    (bimap ((+) $ fst v) ((+) $ snd v) p)
    (bimap (max 0 . (+ (-1))) (+ (-1)) v)

throw :: TargetArea -> Probe -> [Probe]
throw t = takeWhile (stop t) . iterate nextStep
  where
    stop (TargetArea left right bottom top) (Probe (x, y) _)
      | x > right = False
      | y < bottom = False
      | otherwise = True

isIn :: TargetArea -> Probe -> Bool
isIn (TargetArea left right bottom top) (Probe (x, y) _)
  | inRange (left, right) x && inRange (bottom, top) y = True
  | otherwise = False

aim :: TargetArea -> [(Point, [Probe])]
aim t@(TargetArea left right bottom top) =
  filter
    (any (isIn t) . snd)
    $ fmap
      (second (throw t . Probe (0, 0)) . dupe)
      candidats
  where
    rangeX = [0 .. right]
    rangeY = [bottom .. - bottom]
    candidats = concatMap ((`fmap` rangeY) . (,)) rangeX

process1 :: TargetArea -> Int
process1 = maximum . fmap (snd . position) . snd . maximumOn (snd . fst) . aim

run1 :: String -> String
run1 = show . process1 . read

process2 :: TargetArea -> Int
process2 = length . aim

run2 :: String -> String
run2 = show . process2 . read
