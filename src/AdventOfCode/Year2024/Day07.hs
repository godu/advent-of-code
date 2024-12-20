module AdventOfCode.Year2024.Day07
  ( run1,
    run2,
  )
where

import AdventOfCode.Utils (split)
import Control.Applicative (some)
import Debug.Trace (traceShowId)
import GHC.Read (Read (readPrec))
import Text.ParserCombinators.ReadP (char, string)
import Text.ParserCombinators.ReadPrec (lift)

data Equation = Equation {total :: Int, numbers :: [Int]}
  deriving (Show)

instance Read Equation where
  readPrec = do
    total <- readPrec
    _ <- lift $ string ": "
    numbers <- split (lift $ char ' ') readPrec
    return $ Equation total numbers

process1 :: [Equation] -> Int
process1 = sum . map total . filter isValid
  where
    isValid :: Equation -> Bool
    isValid (Equation total numbers) = (== total) `any` candidates numbers
    candidates :: [Int] -> [Int]
    candidates (x : xs) = go x xs
      where
        go :: Int -> [Int] -> [Int]
        go acc [] = [acc]
        go acc (y : ys) = go (acc * y) ys <> go (acc + y) ys

run1 :: String -> String
run1 = show . process1 . fmap read . lines

process2 :: [Equation] -> Int
process2 = sum . map total . filter isValid
  where
    isValid :: Equation -> Bool
    isValid (Equation total numbers) = (== total) `any` candidates numbers
    candidates :: [Int] -> [Int]
    candidates (x : xs) = go x xs
      where
        go :: Int -> [Int] -> [Int]
        go acc [] = [acc]
        go acc (y : ys) = go (acc * y) ys <> go (acc + y) ys <> go (read $ show acc <> show y) ys

run2 :: String -> String
run2 = show . process2 . fmap read . lines
