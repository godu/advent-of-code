{-# LANGUAGE LambdaCase #-}

module AdventOfCode.Year2021.Day10
  ( run1,
    process1,
    run2,
    process2,
  )
where

import Data.List (sort)
import Data.Maybe (mapMaybe)

data State = Complete | Corrupted Char | Incomplete String
  deriving (Show)

parse :: String -> State
parse = go []
  where
    go :: String -> String -> State
    go s ('(' : xs) = go (')' : s) xs
    go s ('[' : xs) = go (']' : s) xs
    go s ('{' : xs) = go ('}' : s) xs
    go s ('<' : xs) = go ('>' : s) xs
    go (')' : s) (')' : xs) = go s xs
    go (']' : s) (']' : xs) = go s xs
    go ('}' : s) ('}' : xs) = go s xs
    go ('>' : s) ('>' : xs) = go s xs
    go _ (')' : _) = Corrupted ')'
    go _ (']' : _) = Corrupted ']'
    go _ ('}' : _) = Corrupted '}'
    go _ ('>' : _) = Corrupted '>'
    go [] [] = Complete
    go s [] = Incomplete s
    go _ _ = Complete

process1 :: [String] -> Int
process1 = sum . fmap (score . parse)
  where
    score :: State -> Int
    score (Corrupted ')') = 3
    score (Corrupted ']') = 57
    score (Corrupted '}') = 1197
    score (Corrupted '>') = 25137
    score _ = 0

run1 :: String -> String
run1 = show . process1 . lines

process2 :: [String] -> Int
process2 = middle . sort . mapMaybe (score . parse)
  where
    middle :: [a] -> a
    middle xs = xs !! (length xs `div` 2)
    score :: State -> Maybe Int
    score (Incomplete xs) = return $ foldl (\a b -> (a * 5) + b) 0 $ score' <$> xs
      where
        score' ')' = 1
        score' ']' = 2
        score' '}' = 3
        score' '>' = 4
        score' _ = 0
    score _ = Nothing

run2 :: String -> String
run2 = show . process2 . lines
