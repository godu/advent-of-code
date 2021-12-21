module AdventOfCode.Year2021.Day18
  ( run1,
    run2,
  )
where

import Data.Maybe (fromMaybe)
import Debug.Trace (traceShow)
import Text.Read (get, readPrec, (<++))

data SnailFishNumber = Value Int | Pair SnailFishNumber SnailFishNumber deriving (Eq, Ord)

mapLeftmost :: (Int -> Int) -> SnailFishNumber -> SnailFishNumber
mapLeftmost f (Value n) = Value (f n)
mapLeftmost f (Pair a b) = Pair (mapLeftmost f a) b

mapRightmost :: (Int -> Int) -> SnailFishNumber -> SnailFishNumber
mapRightmost f (Value n) = Value (f n)
mapRightmost f (Pair a b) = Pair a (mapRightmost f b)

instance Read SnailFishNumber where
  readPrec =
    (Value <$> readPrec)
      <++ ( do
              '[' <- get
              a <- readPrec
              ',' <- get
              b <- readPrec
              ']' <- get
              return $ Pair a b
          )

instance Show SnailFishNumber where
  show (Value n) = show n
  show (Pair a b) = "[" <> show a <> "," <> show b <> "]"

magniture :: SnailFishNumber -> Int
magniture (Value n) = n
magniture (Pair a b) = magniture a * 3 + magniture b * 2

explose :: SnailFishNumber -> Maybe SnailFishNumber
explose v =
  let (n, _, v', _) = explose' v
   in if n == 0 then return v' else Nothing
  where
    explose' (Value a) = (5, Nothing, Value a, Nothing)
    explose' (Pair (Value a) (Value b)) = (4, Just a, Value 0, Just b)
    explose' (Pair a b) =
      let (n, l, c, r) = explose' a
          (n', l', c', r') = explose' b
       in case (n, n') of
            (0, _) -> (0, l, Pair c (mapLeftmost (+ fromMaybe 0 r) b), Nothing)
            (_, 0) -> (0, Nothing, Pair (mapRightmost (+ fromMaybe 0 l') a) c', r')
            (n, n') | n <= n' -> (n - 1, l, Pair c (mapLeftmost (+ fromMaybe 0 r) b), Nothing)
            _ -> (n' - 1, Nothing, Pair (mapRightmost (+ fromMaybe 0 l') a) c', r')

split :: SnailFishNumber -> Maybe SnailFishNumber
split (Value n)
  | n > 9 = return $ Pair (Value q) (Value (q + r))
  | otherwise = Nothing
  where
    (q, r) = n `quotRem` 2
split (Pair a b) = case (split a, split b) of
  (Just a', _) -> return $ Pair a' b
  (_, Just b') -> return $ Pair a b'
  _ -> Nothing

instance Semigroup SnailFishNumber where
  (Value a) <> (Value b) = reduce $ Value (a + b)
  a@(Value _) <> (Pair b c) = reduce $ Pair (a <> b) c
  (Pair a b) <> c@(Value _) = reduce $ Pair a (b <> c)
  a <> b = reduce $ Pair a b

reduce :: SnailFishNumber -> SnailFishNumber
reduce a = case (explose a, split a) of
  (Just a', _) -> reduce a'
  (_, Just a') -> reduce a'
  (Nothing, Nothing) -> a

process1 :: [SnailFishNumber] -> Int
process1 = magniture . foldl1 (<>)

run1 :: String -> String
run1 = show . process1 . fmap read . lines

possibleTuples :: [a] -> [(a, a)]
possibleTuples [] = []
possibleTuples [a] = []
possibleTuples (a : xs) = concatMap (\b -> [(a, b), (b, a)]) xs <> possibleTuples xs

process2 :: [SnailFishNumber] -> Int
process2 = maximum . fmap (process1 . (\(a, b) -> [a, b])) . possibleTuples

run2 :: String -> String
run2 = show . process2 . fmap read . lines
