module AdventOfCode.Year2024.Day02
  ( run1,
    run2,
  )
where

import AdventOfCode.Utils (read', split)
import Data.Ix (Ix (inRange))
import Debug.Trace (traceShowId)
import GHC.Read (readPrec)
import Text.ParserCombinators.ReadP (string)
import Text.ParserCombinators.ReadPrec (lift)
import Text.Read (ReadPrec)
import Prelude hiding (or)

pairs :: [a] -> [(a, a)]
pairs xs = zip xs (tail xs)

readPrecInput :: ReadPrec [Int]
readPrecInput = splitBy " " readPrec
  where
    splitBy = split . lift . string

allIncreasing :: [Int] -> Bool
allIncreasing = all (inRange (1, 3))

allDescreasing :: [Int] -> Bool
allDescreasing = all (inRange (-3, -1))

or a b xs = a xs || b xs

predicate = allIncreasing `or` allDescreasing

toDelta = map (uncurry (-)) . pairs

process1 :: [[Int]] -> Int
process1 = length . filter (predicate . toDelta)

run1 :: String -> String
run1 = show . process1 . map (read' readPrecInput) . lines

possiblities :: [Int] -> [[Int]]
possiblities [] = []
possiblities (x : xs) = [xs] <> ((x :) <$> possiblities xs)

process2 :: [[Int]] -> Int
process2 = length . filter (any (predicate . toDelta) . possiblities)

run2 :: String -> String
run2 = show . process2 . map (read' readPrecInput) . lines
