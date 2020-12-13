module AdventOfCode.Day13
  ( run1,
    process1,
    run2,
    process2,
  )
where

import Data.List.Extra (minimumOn)
import Data.Maybe (catMaybes)
import Text.ParserCombinators.ReadP (eof, sepBy, skipSpaces, string, (+++))
import Text.ParserCombinators.ReadPrec (lift, minPrec, readPrec_to_P)
import Text.Read (Read (readPrec))

data Note = Note Int [Maybe Int] deriving (Show)

instance Read Note where
  readPrec = do
    timestamp <- readPrec
    lift skipSpaces
    lines <-
      lift $
        sepBy
          ( (return <$> readPrec_to_P readPrec minPrec)
              +++ (Nothing <$ string "x")
          )
          (string ",")
    lift skipSpaces
    lift eof
    return $ Note timestamp lines

process1 :: Note -> Int
process1 (Note timestamp lines) =
  (\(line, depart) -> line * (depart - timestamp)) $
    minimumOn snd $
      nextDepart timestamp <$> catMaybes lines
  where
    nextDepart :: Int -> Int -> (Int, Int)
    nextDepart timestamp line = (line, (* line) $ (+ 1) $ timestamp `div` line)

run1 :: String -> String
run1 = show . process1 . read

process2 :: Int -> Int
process2 = id

run2 :: String -> String
run2 = const ""
