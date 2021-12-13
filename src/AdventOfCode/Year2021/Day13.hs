module AdventOfCode.Year2021.Day13
  ( run1,
    run2,
  )
where

import Data.Bifunctor (first, second)
import Data.Char (isDigit)
import Data.Foldable (maximumBy)
import Data.Maybe (isJust)
import Data.Set (Set, fromList, member, toList)
import Debug.Trace (traceShow, traceShowId)
import Text.ParserCombinators.ReadP (choice, many, munch, munch1, string)
import Text.Read (lift, readPrec)
import Prelude hiding (Left)

type Point = (Int, Int)

data Fold = Left Int | Bottom Int deriving (Show)

data Input = Input [Point] [Fold] deriving (Show)

instance Read Input where
  readPrec = do
    points <-
      lift $
        many
          ( do
              x <- munch1 isDigit
              string ","
              y <- munch1 isDigit
              string "\n"
              return (read x, read y)
          )
    lift $ string "\n"
    folds <-
      lift $
        many
          ( do
              string "fold along "
              fold <-
                choice
                  [ do
                      string "y="
                      y <- munch1 isDigit
                      return $ Bottom $ read y,
                    do
                      string "x="
                      x <- munch1 isDigit
                      return $ Left $ read x
                  ]
              string "\n"
              return fold
          )
    return $ Input points folds

generate :: Input -> [Set Point]
generate (Input points folds) = scanl go (fromList points) folds
  where
    go :: Set Point -> Fold -> Set Point
    go m (Bottom n) = fromList $ second (\y -> if y > n then 2 * n - y else y) <$> toList m
    go m (Left n) = fromList $ first (\x -> if x > n then 2 * n - x else x) <$> toList m

run1 :: String -> String
run1 = show . length . (!! 1) . generate . read

printResult :: Set Point -> String
printResult xs = unlines $ (\y -> (\x -> if (x, y) `member` xs then '#' else '.') <$> [0 .. maxX]) <$> [0 .. maxY]
  where
    maxX = maximum $ fst <$> toList xs
    maxY = maximum $ snd <$> toList xs

run2 :: String -> String
run2 = printResult . last . generate . read
