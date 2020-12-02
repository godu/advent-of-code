module AdventOfCode.DayX
  ( run1,
    process1,
    run2,
    process2,
  )
where

import Text.Read (Read (readPrec))

newtype Input = Input Int deriving (Show)

instance Read Input where
  readPrec = Input <$> readPrec

extract :: Input -> Int
extract (Input x) = x

run1 :: String -> String
run1 = show . process1 . extract . read

process1 :: Int -> Int
process1 = id

run2 :: String -> String
run2 = show . process2 . extract . read

process2 :: Int -> Int
process2 = id
