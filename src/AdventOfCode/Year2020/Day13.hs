module AdventOfCode.Year2020.Day13
  ( run1,
    process1,
    run2,
    process2,
  )
where

import Control.Monad.Extra (fold1M)
import Data.List.Extra (minimumOn)
import Data.Maybe (catMaybes, fromJust, fromMaybe, mapMaybe)
import Math.NumberTheory.Moduli.Chinese (chinese)
import Text.ParserCombinators.ReadP (eof, sepBy, skipSpaces, string, (+++))
import Text.ParserCombinators.ReadPrec (lift, minPrec, readPrec_to_P)
import Text.Read (Read (readPrec))

data Note = Note Integer [Maybe Integer] deriving (Show)

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

process1 :: Note -> Integer
process1 (Note timestamp lines) =
  let (line, depart) = findNextDepart timestamp lines
   in line * (depart - timestamp)

findNextDepart :: Integer -> [Maybe Integer] -> (Integer, Integer)
findNextDepart timestamp lines =
  minimumOn snd $
    nextDepart timestamp <$> catMaybes lines
  where
    nextDepart :: Integer -> Integer -> (Integer, Integer)
    nextDepart timestamp line =
      ( line,
        case timestamp `divMod` line of
          (d, 0) -> d * line
          (d, _) -> (d + 1) * line
      )

run1 :: String -> String
run1 = show . process1 . read

process2 :: Note -> Integer
process2 (Note _ lines) =
  uncurry mod $
    fromMaybe (0, 0) $
      fold1M chinese $
        mapMaybe
          ( \(i, line) -> case line of
              Nothing -> Nothing
              Just l -> return (l - i, l)
          )
          (zip [0 ..] lines)

run2 :: String -> String
run2 = show . process2 . read
