module AdventOfCode.Year2021.Day08
  ( run1,
    run2,
  )
where

import Data.Bifunctor (Bifunctor (first), bimap)
import qualified Data.List as L ((\\))
import Data.List.Extra (stripInfix)
import Data.Map (Map, (!), (!?))
import Data.Maybe (fromJust, fromMaybe, isJust)
import Data.Set (Set, delete, elems, fromList, (\\))
import Debug.Trace (traceShow, traceShowId, traceStack)

parse :: String -> ([Set Char], [Set Char])
parse =
  bimap
    (fmap fromList . words)
    (fmap fromList . words)
    . fromMaybe ([], [])
    . stripInfix " | "

process1 :: [([Set Char], [Set Char])] -> Int
process1 = length . filter ((`elem` [2, 3, 4, 7]) . length) . concatMap snd

run1 :: String -> String
run1 =
  show
    . process1
    . fmap parse
    . lines

decode :: [Char] -> Maybe Int
decode ['a', 'b', 'c', 'e', 'f', 'g'] = Just 0
decode ['c', 'f'] = Just 1
decode ['a', 'c', 'd', 'e', 'g'] = Just 2
decode ['a', 'c', 'd', 'f', 'g'] = Just 3
decode ['b', 'c', 'd', 'f'] = Just 4
decode ['a', 'b', 'd', 'f', 'g'] = Just 5
decode ['a', 'b', 'd', 'e', 'f', 'g'] = Just 6
decode ['a', 'c', 'f'] = Just 7
decode ['a', 'b', 'c', 'd', 'e', 'f', 'g'] = Just 8
decode ['a', 'b', 'c', 'd', 'f', 'g'] = Just 9
decode _ = Nothing

only :: [a] -> a
only [a] = a
only _ = error "Only"

process2 :: ([Set Char], [Set Char]) -> Int
process2 (xs, ys) = read $ concatMap (show . toInt) ys
  where
    toInt x
      | x == zero = 0
      | x == one = 1
      | x == two = 2
      | x == three = 3
      | x == four = 4
      | x == five = 5
      | x == six = 6
      | x == seven = 7
      | x == eight = 8
      | otherwise = 9
    a = only $ elems $ seven \\ one
    b = only $ elems $ four \\ three
    c =
      let twoFiveSix = xs L.\\ [zero, one, three, four, seven, eight, nine]
       in only $ filter (\c -> (== 1) $ length $ filter (elem c) twoFiveSix) $ elems eight
    d = only $ elems $ delete g $ three \\ seven
    e = only $ elems $ eight \\ nine
    f = only $ elems $ delete b $ six \\ two
    g = only $ elems $ (three \\ four) \\ seven
    zero = d `delete` eight
    one = only $ filter ((== 2) . length) xs
    two =
      let twoFiveSix = xs L.\\ [zero, one, three, four, seven, eight, nine]
       in only $ filter (elem c) twoFiveSix
    three = only $ filter ((== 3) . length . (\\ one)) $ xs
    four = only $ filter ((== 4) . length) xs
    five =
      let fiveSix = xs L.\\ [zero, one, two, three, four, seven, eight, nine]
       in only $ filter ((== 5) . length) fiveSix
    six =
      let fiveSix = xs L.\\ [zero, one, two, three, four, seven, eight, nine]
       in only $ filter ((== 6) . length) fiveSix
    seven = only $ filter ((== 3) . length) xs
    eight = only $ filter ((== 7) . length) xs
    nine = four <> three

run2 :: String -> String
run2 =
  show
    . sum
    . fmap (process2 . parse)
    . lines
