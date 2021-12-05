{-# LANGUAGE TupleSections #-}

module AdventOfCode.Year2021.Day05
  ( run1,
    process1,
    run2,
    process2,
  )
where

import Data.Function (on)
import Data.Ix (range)
import Data.List (group, sort)
import GHC.Read (readPrec)
import Text.ParserCombinators.ReadP (char, string)
import Text.ParserCombinators.ReadPrec (lift)

type Point = (Int, Int)

data Line = Line Point Point deriving (Show)

instance Read Line where
  readPrec = do
    a <- readPrec
    lift $ char ','
    b <- readPrec
    lift $ string " -> "
    c <- readPrec
    lift $ char ','
    d <- readPrec
    return $ Line (a, b) (c, d)

process1 :: [Line] -> Int
process1 =
  length
    . filter ((>= 2) . length)
    . group
    . sort
    . concatMap points
  where
    points :: Line -> [Point]
    points line@(Line (a, b) (c, d))
      | a == c = (a,) <$> range (b `min` d, b `max` d)
      | b == d = (,b) <$> range (a `min` c, a `max` c)
      | otherwise = []

run1 :: String -> String
run1 = show . process1 . fmap read . lines

process2 :: [Line] -> Int
process2 =
  length
    . filter ((>= 2) . length)
    . group
    . sort
    . concatMap points
  where
    points :: Line -> [Point]
    points line@(Line (a, b) (c, d))
      | a == c = (a,) <$> range' b d
      | b == d = (,b) <$> range' a c
      | abs (a - c) == abs (b - d) = zip (range' a c) (range' b d)
      | otherwise = []
      where
        range' a b = if a < b then range (a, b) else reverse $ range (b, a)

run2 :: String -> String
run2 = show . process2 . fmap read . lines
