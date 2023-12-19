module AdventOfCode.Year2023.Day06
  ( run1,
    run2,
  )
where

import AdventOfCode.Utils (many1, split)
import Data.Char (isDigit)
import GHC.Read (Read (readPrec))
import Text.ParserCombinators.ReadP (char, munch1, string)
import Text.ParserCombinators.ReadPrec (lift)

data Input = Input
  { times :: [Int],
    distances :: [Int]
  }
  deriving (Show)

instance Read Input where
  readPrec = do
    lift $ string "Time:"
    spaces
    time <- split spaces readPrec
    lift $ string "\nDistance:"
    spaces
    distance <- split spaces readPrec
    return $ Input time distance
    where
      spaces = many1 $ lift $ string " "

race :: Int -> Int -> Int
race time holdingTime = restTime * holdingTime
  where
    restTime = time - holdingTime

process1 :: Input -> Int
process1 (Input time distance) = product $ go <$> zip time distance
  where
    go :: (Int, Int) -> Int
    go (time, distance) = length . filter ((> distance) . race time) $ [0 .. time]

run1 :: String -> String
run1 = show . process1 . read

solve :: Double -> Double -> Double -> (Double, Double)
solve a b c = (x1, x2)
  where
    discriminant = b * b - 4 * a * c
    x1 = (-b + sqrt discriminant) / (2 * a)
    x2 = (-b - sqrt discriminant) / (2 * a)

process2 :: Input' -> Int
process2 (Input' time distance) =
  right - left + 1
  where
    (x1, x2) = solve (-1) (fromIntegral time) (-fromIntegral distance)
    (left, right) = (min time (floor x1 + 1), min time (ceiling x2 - 1))

data Input' = Input'
  { time :: Int,
    distance :: Int
  }
  deriving (Show)

instance Read Input' where
  readPrec = do
    lift $ string "Time:"
    spaces
    times <- split spaces digit
    lift $ string "\nDistance:"
    spaces
    distances <- split spaces digit
    return $ Input' (read $ concat times) (read $ concat distances)
    where
      spaces = many1 $ lift $ string " "
      digit = lift $ munch1 isDigit

run2 :: String -> String
run2 = show . process2 . read
