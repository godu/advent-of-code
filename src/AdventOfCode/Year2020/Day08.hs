module AdventOfCode.Year2020.Day08
  ( run1,
    run2,
  )
where

import AdventOfCode.Utils (read')
import Data.List (unfoldr)
import Data.Maybe (mapMaybe)
import Data.Set (Set, empty, insert, member)
import Data.Vector (Vector, fromList, imapMaybe, singleton, toList, update, (!?))
import Text.ParserCombinators.ReadP (skipSpaces, string)
import Text.ParserCombinators.ReadPrec ((+++))
import Text.Read (ReadPrec, lift, readPrec)

data Instruction
  = Nop Int
  | Acc Int
  | Jmp Int
  deriving (Show)

instructionParser :: ReadPrec Instruction
instructionParser =
  ( do
      lift $ string "nop"
      lift skipSpaces
      Nop <$> signedNumberParser
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

execute :: Vector Instruction -> (Int, Int)
execute instructions = last $ haltOnLoop fst $ (0, 0) : unfoldr go (0, 0)
  where
    go :: (Int, Int) -> Maybe ((Int, Int), (Int, Int))
    go (position, value) =
      case instructions !? position of
        Just (Nop _) -> return ((position + 1, value), (position + 1, value))
        Just (Acc x) -> return ((position + 1, value + x), (position + 1, value + x))
        Just (Jmp x) -> return ((position + x, value), (position + x, value))
        Nothing -> Nothing
    haltOnLoop :: (Ord b) => (a -> b) -> [a] -> [a]
    haltOnLoop f xs = haltOnLoop' f xs empty
      where
        haltOnLoop' :: (Ord b) => (a -> b) -> [a] -> Set b -> [a]
        haltOnLoop' _ [] _ = []
        haltOnLoop' f (x : xs) s
          | f x `member` s = [x]
          | otherwise = x : haltOnLoop' f xs (f x `insert` s)

process1 :: Vector Instruction -> Int
process1 = snd . execute

run1 :: String -> String
run1 = show . process1 . fromList . fmap (read' instructionParser) . lines

swap :: Vector Instruction -> Int -> Maybe (Vector Instruction)
swap xs i = do
  x <- xs !? i
  x' <- swap' x
  return $ update xs $ singleton (i, x')
  where
    swap' (Acc _) = Nothing
    swap' (Jmp x) = return $ Nop x
    swap' (Nop x) = return $ Jmp x

process2 :: Vector Instruction -> Int
process2 instructions = head $ mapMaybe terminal possibleSolutions
  where
    possibleSolutions :: [Vector Instruction]
    possibleSolutions = toList $ imapMaybe (\i _ -> instructions `swap` i) instructions
    terminal :: Vector Instruction -> Maybe Int
    terminal instructions =
      let (position, value) = execute instructions
       in if position == length instructions then return value else Nothing

run2 :: String -> String
run2 = show . process2 . fromList . fmap (read' instructionParser) . lines
