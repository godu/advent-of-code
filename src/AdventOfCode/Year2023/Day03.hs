module AdventOfCode.Year2023.Day03
  ( run1,
    run2,
  )
where

import AdventOfCode.Utils (read')
import Data.Char (isDigit)
import Data.Either.Extra (mapLeft)
import Data.Ix (Ix (inRange))
import Data.List (partition)
import Data.Tree (flatten)
import GHC.Read (Read (readPrec))
import Text.ParserCombinators.ReadP (many, sepBy, string)
import Text.ParserCombinators.ReadPrec (ReadPrec, lift, readS_to_Prec)

data Exp = Value Int | Operation Char deriving (Show)

isValue :: Exp -> Bool
isValue (Value _) = True
isValue _ = False

isOperation :: Exp -> Bool
isOperation (Operation _) = True
isOperation _ = False

toInt :: Exp -> Int
toInt (Value x) = x
toInt _ = 0

type Symbol = (Int, Int, Int, Exp)

toExpression :: Symbol -> Exp
toExpression (_, _, _, exp) = exp

type Input = [Symbol]

mapFst :: (a -> c) -> (a, b) -> (c, b)
mapFst f (a, b) = (f a, b)

readLine :: ReadS [(Int, Int, Exp)]
readLine = go 0
  where
    go :: Int -> ReadS [(Int, Int, Exp)]
    go i [] = [([], "")]
    go i ('.' : xs) = go (i + 1) xs
    go i (x : xs)
      | isDigit x = let (ys, rest) = span isDigit xs in mapFst ((i, i + length ys, Value $ read $ x : ys) :) <$> go (i + length ys + 1) rest
      | otherwise = mapFst ((i, i, Operation x) :) <$> go (i + 1) xs

readInput :: ReadS Input
readInput x =
  [ ( concatMap (\(line, xs) -> fmap (\(start, end, exp) -> (line, start, end, exp)) xs) (zip [0 ..] (read' (readS_to_Prec (const readLine)) <$> lines x)),
      ""
    )
  ]

near :: Symbol -> Symbol -> Bool
near (l1, s1, e1, _) (l2, s2, e2, _)
  | l1 == l2 && s1 == s2 && e1 == e2 = False
  | (l1 - 1, l1 + 1) `inRange` l2 && e1 + 1 >= s2 && s1 - 1 <= e2 = True
  | otherwise = False

run1 :: String -> String
run1 = show . process1 . read' (readS_to_Prec (const readInput))

process1 :: Input -> Int
process1 input =
  let (values, operators) = partition (\(_, _, _, exp) -> isValue exp) input
   in sum $
        (\(_, _, _, exp) -> toInt exp)
          <$> filter
            ( \symbol ->
                (/= 0) $
                  length $
                    filter (near symbol) operators
            )
            values

run2 :: String -> String
run2 = show . process2 . read' (readS_to_Prec (const readInput))

isGear :: Exp -> Bool
isGear (Operation '*') = True
isGear _ = False

process2 :: Input -> Int
process2 input =
  let (values, operators) = partition (\(_, _, _, exp) -> isValue exp) input
   in sum $
        fmap (product . fmap (toInt . toExpression)) $
          filter ((== 2) . length) $
            ( (\symbol -> filter (near symbol) values)
                <$> filter
                  (\(_, _, _, exp) -> isGear exp)
                  operators
            )
