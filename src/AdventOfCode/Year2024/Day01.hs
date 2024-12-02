module AdventOfCode.Year2024.Day01
  ( run1,
    run2,
  )
where

import Data.Bifunctor (Bifunctor (bimap))
import Data.List (sort)
import Data.Map (singleton, unionsWith, (!?))
import Data.Maybe (fromMaybe)
import GHC.Read (Read (readPrec))
import Text.ParserCombinators.ReadPrec (ReadPrec)

newtype Input = Input (Int, Int)

instance Read Input where
  readPrec = do
    a <- readPrec
    b <- readPrec
    return $ Input (a, b)

process1 :: [(Int, Int)] -> Int
process1 =
  sum
    . map (abs . uncurry (-))
    . uncurry zip
    . bimap sort sort
    . unzip

run1 :: String -> String
run1 = show . process1 . map ((\(Input a) -> a) . read) . lines

process2 :: [(Int, Int)] -> Int
process2 xs = sum $ (\x -> x * fromMaybe 0 ((!?) xx $ x)) <$> xs1
  where
    (xs1, xs2) = unzip xs
    xx = unionsWith (+) $ (`singleton` 1) <$> xs2

run2 :: String -> String
run2 = show . process2 . map ((\(Input a) -> a) . read) . lines
