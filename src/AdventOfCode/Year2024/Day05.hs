{-# LANGUAGE TupleSections #-}

module AdventOfCode.Year2024.Day05
  ( run1,
    run2,
  )
where

import AdventOfCode.Utils (read', split)
import Data.List (sortBy, tails)
import Data.Set (Set, fromList)
import Data.Tuple (swap)
import Debug.Trace (traceShow, traceShowId)
import GHC.Read (Read (readPrec))
import Text.ParserCombinators.ReadP (char, eof)
import Text.ParserCombinators.ReadPrec (ReadPrec, lift)

data Puzzle a = Puzzle
  { orderingRules :: Set (a, a),
    updateList :: [[a]]
  }
  deriving (Show, Read)

readPrecRule :: (Read a) => ReadPrec (a, a)
readPrecRule = do
  a <- readPrec
  _ <- lift $ char '|'
  b <- readPrec
  return (a, b)

readPrecUpdate :: (Read a) => ReadPrec [a]
readPrecUpdate = split (lift $ char ',') readPrec

readPrecPuzzle :: (Ord a, Read a) => ReadPrec (Puzzle a)
readPrecPuzzle = do
  orderingRules <- split (lift $ char '\n') readPrecRule
  lift $ char '\n'
  updateList <- split (lift $ char '\n') readPrecUpdate
  lift eof
  return
    Puzzle
      { orderingRules = fromList orderingRules,
        updateList = filter (not . null) updateList
      }

isCorrect :: (Eq a, Show a) => Set (a, a) -> [a] -> Bool
isCorrect rules = all (go rules) . tails
  where
    go :: (Eq a, Show a) => Set (a, a) -> [a] -> Bool
    go _ [] = True
    go rules (x : xs) = all ((`elem` rules) . (x,)) xs

middle :: [a] -> a
middle xs = xs !! (length xs `div` 2)

process1 :: Puzzle Int -> Int
process1 (Puzzle orderingRules updateList) =
  sum $ map middle $ filter (isCorrect orderingRules) updateList

run1 :: String -> String
run1 = show . process1 . read' readPrecPuzzle

comparePage :: (Eq a, Show a) => Set (a, a) -> a -> a -> Ordering
comparePage rules a b
  | (a, b) `elem` rules = LT
  | (b, a) `elem` rules = GT
  | otherwise = EQ

process2 :: Puzzle Int -> Int
process2 (Puzzle orderingRules updateList) =
  sum $ map middle $ correctUpdates
  where
    incorrectUpdates = filter (not . isCorrect orderingRules) updateList
    correctUpdates = sortBy (comparePage orderingRules) <$> incorrectUpdates

run2 :: String -> String
run2 = show . process2 . read' readPrecPuzzle
