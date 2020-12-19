module AdventOfCode.Day18
  ( run1,
    process1,
    run2,
    process2,
  )
where

import AdventOfCode.Utils (read')
import Data.Char (isDigit)
import Debug.Trace
import Text.ParserCombinators.ReadP (ReadP, chainl, chainl1, eof, get, munch1, skipSpaces, (+++), (<++))
import Text.ParserCombinators.ReadPrec (lift, pfail)
import Text.Read (Read (readPrec), ReadPrec)

data Expression
  = Value Int
  | Addition Expression Expression
  | Multiplication Expression Expression
  deriving (Show)

add :: ReadP (Expression -> Expression -> Expression)
add = do
  '+' <- get
  skipSpaces
  return Addition

mul :: ReadP (Expression -> Expression -> Expression)
mul = do
  '*' <- get
  skipSpaces
  return Multiplication

value :: ReadP Expression
value = Value . read <$> munch1 isDigit <* skipSpaces

paren :: ReadP b -> ReadP b
paren a = do
  '(' <- get
  skipSpaces
  a' <- a
  ')' <- get
  skipSpaces
  return a'

leftRight :: ReadPrec Expression
leftRight = lift exp <* lift eof
  where
    exp :: ReadP Expression
    exp = chainl1 (value <++ paren exp) (add +++ mul)

reduce :: Expression -> Int
reduce (Value a) = a
reduce (Addition a b) = reduce a + reduce b
reduce (Multiplication a b) = reduce a * reduce b

process1 :: Int -> Int
process1 = id

run1 :: String -> String
run1 = show . sum . fmap (reduce . read' leftRight) . lines

process2 :: Int -> Int
process2 = id

run2 :: String -> String
run2 = const ""
