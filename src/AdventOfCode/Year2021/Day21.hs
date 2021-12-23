{-# LANGUAGE TupleSections #-}

module AdventOfCode.Year2021.Day21
  ( run1,
    run2,
  )
where

import AdventOfCode.Utils (split)
import Data.Bifunctor (bimap)
import Data.List (find)
import Data.Map (singleton, toList, unionsWith)
import Data.Maybe (fromJust)
import Debug.Trace (traceShow, traceShowId)
import Text.ParserCombinators.ReadP (string)
import Text.Read (get, lift, readPrec)

type Position = Int

type Score = Int

data Game = Game {rollCount :: Int, players :: (Player, Player)} deriving (Eq)

instance Show Game where
  show (Game rollCount players) =
    concat
      [ "Game {rollCount = ",
        show rollCount,
        ", players = ",
        show players,
        "}"
      ]

instance Read Game where
  readPrec = do
    player1 <- readPrec
    '\n' <- get
    player2 <- readPrec
    return $ Game 0 (player1, player2)

data Player = Player {identifier :: Int, position :: Int, score :: Int} deriving (Show, Eq)

instance Read Player where
  readPrec = do
    lift $ string "Player "
    identifier <- readPrec
    lift $ string " starting position: "
    position <- readPrec
    return $ Player identifier position 0

move :: Player -> Int -> Player
move (Player i p s) n = Player i space (s + space)
  where
    space = (p + n - 1) `mod` 10 + 1

process1 :: Game -> Int
process1 =
  maybe 0 compute
    . find (any ((>= 1000) . score) . players)
    . fmap snd
    . iterate turn
    . (cycle [1 .. 100],)
  where
    compute :: Game -> Int
    compute (Game rollCount (Player _ _ score, _)) = rollCount * score

    turn :: ([Int], Game) -> ([Int], Game)
    turn (f : s : t : dice, Game rollCount (current, other)) =
      (dice, Game (rollCount + 3) (other, move current (f + s + t)))
    turn x = x

run1 :: String -> String
run1 = show . process1 . read

process2 :: Game -> Int
process2 g = go (0, 0) [(g, 1)]
  where
    rolls = [(3, 1), (4, 3), (5, 6), (6, 7), (7, 6), (8, 3), (9, 1)]
    turn :: Int -> Game -> Game
    turn n (Game rollCount (current, other)) =
      Game (rollCount + 3) (other, move current n)
    score (Game _ (Player i _ s, _)) | s >= 21 = Just $ if i == 1 then (1, 0) else (0, 1)
    score (Game _ (_, Player i _ s)) | s >= 21 = Just $ if i == 1 then (1, 0) else (0, 1)
    score _ = Nothing
    go :: (Int, Int) -> [(Game, Int)] -> Int
    go (one, two) ((cur, n) : rest) = case score cur of
      Nothing -> go (one, two) $ (bimap (`turn` cur) (* n) <$> rolls) <> rest
      Just (one', two') -> go (one + (n * one'), two + (n * two')) rest
    go (one, two) [] = max one two

run2 :: String -> String
run2 = show . process2 . read
