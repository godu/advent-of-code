module AdventOfCode.Day02
  ( Input (Input),
    run1,
    process1,
    run2,
    process2,
  )
where

import Data.Ix
  ( Ix (inRange),
  )
import Text.Read
  ( Lexeme (Ident, Symbol),
    Read (readPrec),
    lexP,
  )

data Input = Input
  { min :: Int,
    max :: Int,
    char :: Char,
    password :: String
  }
  deriving (Show)

instance Read Input where
  readPrec = do
    min <- readPrec
    Symbol "-" <- lexP
    max <- readPrec
    Ident [char] <- lexP
    Symbol ":" <- lexP
    Ident password <- lexP
    return $ Input min max char password

run1 :: String -> String
run1 = show . process1 . fmap (read :: String -> Input) . lines

process1 :: [Input] -> Int
process1 = length . filter isValid
  where
    isValid :: Input -> Bool
    isValid (Input min max char password) = inRange (min, max) count
      where
        count = length $ filter (== char) password

run2 :: String -> String
run2 = show . process2 . fmap (read :: String -> Input) . lines

process2 :: [Input] -> Int
process2 = length . filter isValid
  where
    isValid :: Input -> Bool
    isValid (Input min max char password) = minIsValid /= maxIsValid
      where
        minIsValid = (min <= length password) && char == (password !! (min - 1))
        maxIsValid = (max <= length password) && char == (password !! (max - 1))
