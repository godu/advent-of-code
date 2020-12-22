module AdventOfCode.Day22
  ( run1,
    run2,
  )
where

import Text.ParserCombinators.ReadP (eof, sepBy, skipSpaces, string)
import Text.Read (Read (readPrec), lift, minPrec, readPrec_to_P)

type Card = Int

data Player = Player Int [Card] deriving (Show)

instance Read Player where
  readPrec = do
    lift $ string "Player "
    id <- readPrec
    lift $ string ":\n"
    cards <- lift $ sepBy (readPrec_to_P readPrec minPrec) (string "\n")
    return $ Player id cards

data Game = Game Player Player deriving (Show)

instance Read Game where
  readPrec = do
    playerA <- readPrec
    lift $ string "\n\n"
    playerB <- readPrec
    lift skipSpaces
    lift eof
    return $ Game playerA playerB

answer :: [Card] -> Int
answer = sum . fmap (uncurry (*)) . zip [1 ..] . reverse

process1 :: Game -> Int
process1 (Game (Player _ []) (Player _ bs)) = answer bs
process1 (Game (Player _ as) (Player _ [])) = answer as
process1 (Game (Player idA (a : as)) (Player idB (b : bs))) =
  if a <= b
    then process1 (Game (Player idA as) (Player idB (bs <> [b, a])))
    else process1 (Game (Player idA (as <> [a, b])) (Player idB bs))

run1 :: String -> String
run1 = show . process1 . read

process2 :: Int -> Int
process2 = id

run2 :: String -> String
run2 = const ""
