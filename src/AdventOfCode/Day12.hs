module AdventOfCode.Day12
  ( run1,
    process1,
    run2,
    process2,
  )
where

data Instruction
  = North Int
  | South Int
  | East Int
  | West Int
  | Left Int
  | Right Int
  | Forward Int

process1 :: Int -> Int
process1 = id

run1 :: String -> String
run1 = const ""

process2 :: Int -> Int
process2 = id

run2 :: String -> String
run2 = const ""
