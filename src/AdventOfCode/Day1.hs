{-# LANGUAGE TupleSections #-}

module AdventOfCode.Day1
  ( run,
    process,
  )
where

import Data.List (sortOn)
import Text.ParserCombinators.ReadPrec ((+++))
import Text.Read (Read (readPrec))

newtype Input = Input [Int] deriving (Show)

instance Read Input where
  readPrec =
    ( do
        a <- readPrec
        Input as <- readPrec
        return $ Input (a : as)
    )
      +++ ( do
              return (Input [])
          )

combinaisons :: [a] -> [(a, a)]
combinaisons [] = []
combinaisons (a : as) = map (a,) as <> combinaisons as

run :: String -> String
run = show . sum . fmap (uncurry (*)) . filter ((==) 2020 . uncurry (+)) . combinaisons . (\(Input x) -> x) . read

process :: [Int] -> Int
process =
  sum
    . fmap (uncurry (*))
    . filter ((==) 2020 . uncurry (+))
    . combinaisons
