module AdventOfCode.Year2023.Day01
  ( run1,
    run2,
  )
where

process1 :: [[Int]] -> Int
process1 = sum . map (\xs -> head xs * 10 + last xs)

run1 :: String -> String
run1 = show . process1 . map parse . lines
  where
    parse :: String -> [Int]
    parse [] = []
    parse ('0' : xs) = 0 : parse xs
    parse ('1' : xs) = 1 : parse xs
    parse ('2' : xs) = 2 : parse xs
    parse ('3' : xs) = 3 : parse xs
    parse ('4' : xs) = 4 : parse xs
    parse ('5' : xs) = 5 : parse xs
    parse ('6' : xs) = 6 : parse xs
    parse ('7' : xs) = 7 : parse xs
    parse ('8' : xs) = 8 : parse xs
    parse ('9' : xs) = 9 : parse xs
    parse (_ : xs) = parse xs

process2 :: Int -> Int
process2 = id

run2 :: String -> String
run2 = show . process1 . map parse . lines
  where
    parse :: String -> [Int]
    parse [] = []
    parse ('0' : xs) = 0 : parse xs
    parse ('1' : xs) = 1 : parse xs
    parse ('2' : xs) = 2 : parse xs
    parse ('3' : xs) = 3 : parse xs
    parse ('4' : xs) = 4 : parse xs
    parse ('5' : xs) = 5 : parse xs
    parse ('6' : xs) = 6 : parse xs
    parse ('7' : xs) = 7 : parse xs
    parse ('8' : xs) = 8 : parse xs
    parse ('9' : xs) = 9 : parse xs
    parse ('o' : 'n' : 'e' : xs) = 1 : parse ('n' : 'e' : xs)
    parse ('t' : 'w' : 'o' : xs) = 2 : parse ('w' : 'o' : xs)
    parse ('t' : 'h' : 'r' : 'e' : 'e' : xs) = 3 : parse ('h' : 'r' : 'e' : 'e' : xs)
    parse ('f' : 'o' : 'u' : 'r' : xs) = 4 : parse ('o' : 'u' : 'r' : xs)
    parse ('f' : 'i' : 'v' : 'e' : xs) = 5 : parse ('i' : 'v' : 'e' : xs)
    parse ('s' : 'i' : 'x' : xs) = 6 : parse ('i' : 'x' : xs)
    parse ('s' : 'e' : 'v' : 'e' : 'n' : xs) = 7 : parse ('e' : 'v' : 'e' : 'n' : xs)
    parse ('e' : 'i' : 'g' : 'h' : 't' : xs) = 8 : parse ('i' : 'g' : 'h' : 't' : xs)
    parse ('n' : 'i' : 'n' : 'e' : xs) = 9 : parse ('i' : 'n' : 'e' : xs)
    parse (_ : xs) = parse xs
