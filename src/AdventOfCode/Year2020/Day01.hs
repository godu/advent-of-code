module AdventOfCode.Year2020.Day01
  ( run1,
    process1,
    run2,
    process2,
    combinaisons,
  )
where

import Text.Read (Read (readPrec))

newtype Input = Input Int deriving (Show)

instance Read Input where
  readPrec = Input <$> readPrec

extract :: Input -> Int
extract (Input x) = x

combinaisons :: Int -> [a] -> [[a]]
combinaisons _ [] = []
combinaisons 0 xs = []
combinaisons 1 xs = map (: []) xs
combinaisons i (a : as) = map (a :) (combinaisons (i -1) as) <> combinaisons i as

run1 :: String -> String
run1 = show . process1 . fmap (extract . read) . lines

process1 :: [Int] -> Int
process1 =
  sum
    . fmap product
    . filter ((==) 2020 . sum)
    . combinaisons 2

run2 :: String -> String
run2 = show . process2 . fmap (extract . read) . lines

process2 :: [Int] -> Int
process2 =
  sum
    . fmap product
    . filter ((==) 2020 . sum)
    . combinaisons 3
