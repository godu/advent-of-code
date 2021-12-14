{-# LANGUAGE TupleSections #-}

module AdventOfCode.Year2021.Day14
  ( run1,
    run2,
  )
where

import AdventOfCode.Utils (nTimes, singleton)
import Data.Bifunctor (bimap, second)
import Data.Char (isLetter, isUpper)
import Data.Map (Map, fromList, mapWithKey, unionsWith, (!))
import Debug.Trace (traceShow, traceShowId)
import Text.ParserCombinators.ReadP (many1, munch1, satisfy, string)
import Text.Read (lift, readPrec)

data Input = Input String (Map (Char, Char) Char) deriving (Show)

instance Read Input where
  readPrec = do
    template <- lift $ munch1 isLetter
    lift $ string "\n\n"
    rules <-
      lift $
        many1
          ( do
              a <- satisfy isLetter
              b <- satisfy isLetter
              string " -> "
              c <- satisfy isLetter
              string "\n"
              return ((a, b), c)
          )
    return $ Input template (fromList rules)

process :: Int -> Input -> Int
process n input@(Input template rules) =
  result template $
    nTimes (n - 1) (polymerization rules) $
      mapWithKey countLetter rules
  where
    polymerization :: Map (Char, Char) Char -> Map (Char, Char) (Map Char Int) -> Map (Char, Char) (Map Char Int)
    polymerization rules counts = mapWithKey go counts
      where
        go (a, b) v = let c = rules ! (a, b) in unionsWith (+) $ (counts !) <$> [(a, c), (c, b)]
    countLetter :: (Char, Char) -> Char -> Map Char Int
    countLetter (_, b) c = unionsWith (+) $ fmap (fromList . singleton . (,1)) [b, c]
    dup a = (a, a)
    result :: String -> Map (Char, Char) (Map Char Int) -> Int
    result template@(firstTemplate : restTemplate) counts =
      ( uncurry (-)
          . bimap maximum minimum
          . dup
          . unionsWith (+)
      )
        $ (fromList [(firstTemplate, 1)] :) $ (counts !) <$> zip template restTemplate
    result _ _ = 0

run1 :: String -> String
run1 = show . process 10 . read

run2 :: String -> String
run2 = show . process 40 . read
