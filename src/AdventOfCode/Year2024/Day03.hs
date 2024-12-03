{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use lambda-case" #-}
module AdventOfCode.Year2024.Day03
  ( run1,
    run2,
  )
where

import AdventOfCode.Utils (many1, read')
import Control.Applicative (many)
import Data.Char (isDigit)
import Data.List (singleton)
import Text.ParserCombinators.ReadP (eof, munch1, satisfy, skipMany, string)
import Text.ParserCombinators.ReadPrec (ReadPrec, get, (<++))
import Text.Read (Read (readPrec), lift)

data Instruction
  = Mul Int Int
  | Do
  | Dont

readPrecInstructions :: ReadPrec [Instruction]
readPrecInstructions =
  ( do
      instruction <- readPrecMul <++ readPrecDo <++ readPrecDont
      instructions <- readPrecInstructions
      return $ instruction : instructions
  )
    <++ ( do
            get
            readPrecInstructions
        )
    <++ ( do
            lift eof
            return []
        )
  where
    readPrecMul = lift $ do
      string "mul("
      a <- read <$> munch1 isDigit
      string ","
      b <- read <$> munch1 isDigit
      string ")"
      return $ Mul a b
    readPrecDo = lift $ do
      string "do()"
      return Do
    readPrecDont = lift $ do
      string "don't()"
      return Dont

process1 :: [Instruction] -> Int
process1 =
  sum
    . map
      ( \x -> case x of
          Mul a b -> a * b
          _ -> 0
      )

run1 :: String -> String
run1 = show . process1 . read' readPrecInstructions

process2 :: [Instruction] -> Int
process2 = process1 . do_
  where
    do_ (Dont : xs) = dont xs
    do_ (x : xs) = x : do_ xs
    do_ [] = []
    dont (Do : xs) = do_ xs
    dont (x : xs) = dont xs
    dont [] = []

run2 :: String -> String
run2 = show . process2 . read' readPrecInstructions
