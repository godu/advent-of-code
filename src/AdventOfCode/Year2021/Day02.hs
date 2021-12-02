module AdventOfCode.Year2021.Day02
  ( run1,
    process1,
    run2,
    process2,
  )
where

import AdventOfCode.Utils (read')
import Text.ParserCombinators.ReadP (skipSpaces, string)
import Text.ParserCombinators.ReadPrec ((+++))
import Text.Read (ReadPrec, lift, readPrec)

data Instruction
  = Forward Int
  | Up Int
  | Down Int
  deriving (Show)

instructionParser :: ReadPrec Instruction
instructionParser =
  ( do
      lift $ string "forward"
      lift skipSpaces
      Forward <$> readPrec
  )
    +++ ( do
            lift $ string "up"
            lift skipSpaces
            Up <$> readPrec
        )
    +++ ( do
            lift $ string "down"
            lift skipSpaces
            Down <$> readPrec
        )

process1 :: [Instruction] -> Int
process1 = uncurry (*) . foldl go (0, 0)
  where
    go (horizontal, deep) (Forward n) = (horizontal + n, deep)
    go (horizontal, deep) (Up n) = (horizontal, deep - n)
    go (horizontal, deep) (Down n) = (horizontal, deep + n)

run1 :: String -> String
run1 = show . process1 . fmap (read' instructionParser) . lines

process2 :: [Instruction] -> Int
process2 = (\(horizontal, deep, aim) -> horizontal * deep) . foldl go (0, 0, 0)
  where
    go (horizontal, deep, aim) (Forward n) = (horizontal + n, deep + (aim * n), aim)
    go (horizontal, deep, aim) (Up n) = (horizontal, deep, aim - n)
    go (horizontal, deep, aim) (Down n) = (horizontal, deep, aim + n)

run2 :: String -> String
run2 = show . process2 . fmap (read' instructionParser) . lines
