{-# LANGUAGE TupleSections #-}

module AdventOfCode.Year2023.Day04
  ( run1,
    run2,
  )
where

import Data.List (intersect, (!!))
import Text.ParserCombinators.ReadP (many, sepBy, string)
import Text.ParserCombinators.ReadPrec (ReadPrec, lift, minPrec, readPrec_to_P)
import Text.Read (Read (readPrec))

data Card = Card
  { cardId :: Int,
    winningNumbers :: [Int],
    numbersYouHave :: [Int]
  }
  deriving (Show, Eq)

instance Read Card where
  readPrec = do
    lift $ string "Card "
    i <- readPrec
    lift $ string ": "
    ws <- lift $ sepBy (readPrec_to_P readPrec minPrec) (string " ")
    lift $ string " | "
    hs <- lift $ sepBy (readPrec_to_P readPrec minPrec) (string " ")
    return $ Card {cardId = i, winningNumbers = ws, numbersYouHave = hs}

appearsInBoth :: Card -> [Int]
appearsInBoth (Card _ winningNumbers numbersYouHave) = winningNumbers `intersect` numbersYouHave

process1 :: [Card] -> Int
process1 = sum . fmap ((scores !!) . length . appearsInBoth)
  where
    scores = (0 :) $ (2 ^) <$> [0 ..]

run1 :: String -> String
run1 = show . process1 . map read . lines

process2 :: [Card] -> Int
process2 = sum . go . (repeat 1,)
  where
    go :: ([Int], [Card]) -> [Int]
    go (count : remainers, card : cards) = count : go (nextRemainers, cards)
      where
        commons = length $ appearsInBoth card
        nextRemainers = zipWith (+) remainers (replicate commons count ++ repeat 0)
    go _ = []

run2 :: String -> String
run2 = show . process2 . map read . lines
