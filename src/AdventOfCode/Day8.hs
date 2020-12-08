{-# LANGUAGE TupleSections #-}

module AdventOfCode.Day8
  ( run1,
    process1,
    run2,
    process2,
  )
where

import AdventOfCode.Utils (read')
import Data.List (unfoldr)
import Data.Set (Set, empty, insert, member)
import Text.ParserCombinators.ReadP (skipSpaces, string)
import Text.ParserCombinators.ReadPrec ((+++))
import Text.Read (ReadPrec, lift, readPrec)

data Instruction
  = Nop
  | Acc Int
  | Jmp Int
  deriving (Show)

instructionParser :: ReadPrec Instruction
instructionParser =
  ( do
      lift $ string "nop"
      lift skipSpaces
      signedNumberParser
      return Nop
  )
    +++ ( do
            lift $ string "acc"
            lift skipSpaces
            Acc <$> signedNumberParser
        )
    +++ ( do
            lift $ string "jmp"
            lift skipSpaces
            Jmp <$> signedNumberParser
        )
  where
    signedNumberParser :: ReadPrec Int
    signedNumberParser =
      ( do
          lift $ string "+"
          readPrec
      )
        +++ readPrec

run1 :: String -> String
run1 = show . process1 . fmap (read' instructionParser) . lines

dup :: Ord b => (a -> b) -> [a] -> Maybe a
dup f xs = dup' f xs empty
  where
    dup' :: Ord b => (a -> b) -> [a] -> Set b -> Maybe a
    dup' _ [] _ = Nothing
    dup' f (x : xs) s
      | f x `member` s = return x
      | otherwise = dup' f xs (f x `insert` s)

process1 :: [Instruction] -> Int
process1 instructions = maybe 0 snd $ dup fst $ unfoldr go (0, 0)
  where
    go :: (Int, Int) -> Maybe ((Int, Int), (Int, Int))
    go (position, value) =
      return $
        ((position, value),) $
          case instructions !! position of
            Nop -> (position + 1, value)
            Acc x -> (position + 1, value + x)
            Jmp x -> (position + x, value)

run2 :: String -> String
run2 = id

process2 :: Int -> Int
process2 = id
