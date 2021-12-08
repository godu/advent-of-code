module AdventOfCode.Year2021.Day08
  ( run1,
    run2,
  )
where

import Data.Bifunctor (Bifunctor (first), bimap)
import Data.List (permutations, sort)
import Data.List.Extra (stripInfix)
import Data.Map (Map, fromList, (!), (!?))
import Data.Maybe (fromMaybe, isJust)
import Debug.Trace (traceShow, traceShowId)

decode :: [Char] -> Maybe Int
decode ['a', 'b', 'c', 'e', 'f', 'g'] = Just 0
decode ['c', 'f'] = Just 1
decode ['a', 'c', 'd', 'e', 'g'] = Just 2
decode ['a', 'c', 'd', 'f', 'g'] = Just 3
decode ['b', 'c', 'd', 'f'] = Just 4
decode ['a', 'b', 'd', 'f', 'g'] = Just 5
decode ['a', 'b', 'd', 'e', 'f', 'g'] = Just 6
decode ['a', 'c', 'f'] = Just 7
decode ['a', 'b', 'c', 'd', 'e', 'f', 'g'] = Just 8
decode ['a', 'b', 'c', 'd', 'f', 'g'] = Just 9
decode _ = Nothing

process :: ([String], [String]) -> [Int]
process (xs, ys) =
  head $
    (`solve` ys)
      <$> filter
        (match xs)
        solutions
  where
    solutions :: [Map Char Char]
    solutions = fromList . zip "abcdefg" <$> permutations "abcdefg"
    r = sort ["abcefg", "cf", "acdeg", "acdfg", "bcdf", "abdfg", "abdefg", "acf", "abcdefg", "abcdfg"]
    match :: [String] -> Map Char Char -> Bool
    match xs t = (Just r ==) $ fmap sort $ fmap sort <$> mapM (mapM (t !?)) xs
    solve :: Map Char Char -> [String] -> [Int]
    solve t xs = fromMaybe [] $ (mapM decode . fmap sort =<< mapM (mapM (t !?)) xs)

run1 :: String -> String
run1 =
  show
    . length
    . filter (`elem` [1, 4, 7, 8])
    . concatMap
      ( (process . bimap words words)
          . fromMaybe ([], [])
          . stripInfix " | "
      )
    . lines

run2 :: String -> String
run2 =
  show
    . sum
    . fmap
      ( read
          . concatMap show
          . (process . bimap words words)
          . fromMaybe ([], [])
          . stripInfix " | "
      )
    . lines
