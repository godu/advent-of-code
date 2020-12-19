module AdventOfCode.Day18
  ( run1,
    process1,
    run2,
    process2,
  )
where

import AdventOfCode.Utils (read')
import Data.Char (isDigit)
import Text.ParserCombinators.ReadP (ReadP, chainl1, eof, get, munch1, skipSpaces, (+++), (<++))
import Text.ParserCombinators.ReadPrec (ReadPrec, lift)

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
leftRight = lift $ exp <* eof
  where
    exp :: ReadP Expression
    exp = chainl1 (value <++ paren exp) (add +++ mul)

reduce :: Expression -> Int
reduce (Value a) = a
reduce (Addition a b) = reduce a + reduce b
reduce (Multiplication a b) = reduce a * reduce b

process1 :: [Expression] -> Int
process1 = sum . fmap reduce

run1 :: String -> String
run1 = show . process1 . fmap (read' leftRight) . lines

prioritized :: ReadPrec Expression
prioritized = lift $ exp' <* eof
  where
    exp :: ReadP Expression
    exp = chainl1 (value <++ paren exp') add
    exp' :: ReadP Expression
    exp' = chainl1 (exp <++ paren exp') mul

process2 :: [Expression] -> Int
process2 = process1

run2 :: String -> String
run2 = show . process2 . fmap (read' prioritized) . lines
