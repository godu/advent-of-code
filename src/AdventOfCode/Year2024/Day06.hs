{-# LANGUAGE LambdaCase #-}

module AdventOfCode.Year2024.Day06
  ( run1,
    run2,
  )
where

import AdventOfCode.Utils (read')
import Control.Parallel.Strategies (parMap, rpar)
import Data.List (elem, find, nub, nubBy, unfoldr)
import Data.Maybe (fromJust)
import Data.Set (Set, delete, difference, elems, empty, findMin, fromList, insert)
import Data.Tuple.Extra (dupe, first)
import Debug.Trace (traceShow, traceShowId)
import GHC.Read (Read (readPrec))
import Text.ParserCombinators.ReadP (ReadP, char, eof, many1, readP_to_S, readS_to_P, sepBy1)
import Text.ParserCombinators.ReadPrec (ReadPrec, lift, readPrec_to_P)
import Prelude hiding (Left, Right)

type Position = (Int, Int)

data Direction = Up | Down | Left | Right deriving (Eq, Ord, Show)

turn :: Guard -> Guard
turn (Guard position Up) = Guard position Right
turn (Guard position Down) = Guard position Left
turn (Guard position Left) = Guard position Up
turn (Guard position Right) = Guard position Down

instance Read Direction where
  readsPrec _ ('^' : xs) = [(Up, xs)]
  readsPrec _ ('v' : xs) = [(Down, xs)]
  readsPrec _ ('<' : xs) = [(Left, xs)]
  readsPrec _ ('>' : xs) = [(Right, xs)]
  readsPrec _ _ = []

data Guard = Guard
  { position :: Position,
    direction :: Direction
  }
  deriving (Eq, Ord, Show)

data Puzzle = Puzzle
  { walls :: Set Position,
    limit :: Position,
    guard :: Guard
  }
  deriving (Eq, Show)

data Cell = Empty | Wall | Arrow Direction deriving (Eq, Show)

instance Read Cell where
  readsPrec _ ('#' : xs) = [(Wall, xs)]
  readsPrec _ ('.' : xs) = [(Empty, xs)]
  readsPrec _ ('<' : xs) = [(Arrow Left, xs)]
  readsPrec _ ('>' : xs) = [(Arrow Right, xs)]
  readsPrec _ ('^' : xs) = [(Arrow Up, xs)]
  readsPrec _ ('v' : xs) = [(Arrow Down, xs)]
  readsPrec _ _ = []

index2DArray :: [[a]] -> [((Int, Int), a)]
index2DArray =
  concatMap
    (\(i, row) -> zipWith (\j val -> ((i, j), val)) [0 ..] row)
    . zip [0 ..]

readPrecPuzzle :: ReadPrec Puzzle
readPrecPuzzle = do
  cells <- fmap index2DArray $ lift $ many1 readPCell `sepBy1` char '\n'
  let walls = fromList $ fst <$> filter (\(_, a) -> case a of Wall -> True; _ -> False) cells
  let limit = foldr (\(x, y) (x', y') -> (max x x', max y y')) (0, 0) walls
  let guard = fromJust $ find ((\case Arrow _ -> True; _ -> False) . snd) cells
  return
    Puzzle
      { walls = walls,
        limit = limit,
        guard = Guard {position = fst guard, direction = (\(Arrow d) -> d) $ snd guard}
      }
  where
    readPCell :: ReadP Cell
    readPCell = readPrec_to_P readPrec 0

outOfBounds :: Position -> Position -> Bool
outOfBounds (x, y) (x', y') = x' < 0 || y' < 0 || x' > x || y' > y

forward :: Guard -> Guard
forward (Guard (x, y) Up) = Guard (x - 1, y) Up
forward (Guard (x, y) Down) = Guard (x + 1, y) Down
forward (Guard (x, y) Left) = Guard (x, y - 1) Left
forward (Guard (x, y) Right) = Guard (x, y + 1) Right

backward :: Guard -> Guard
backward (Guard (x, y) Up) = Guard (x + 1, y) Up
backward (Guard (x, y) Down) = Guard (x - 1, y) Down
backward (Guard (x, y) Left) = Guard (x, y + 1) Left
backward (Guard (x, y) Right) = Guard (x, y - 1) Right

generation :: Puzzle -> [Puzzle]
generation =
  takeWhile (\(Puzzle _ limit (Guard position _)) -> not $ outOfBounds limit position)
    . unfoldr walk
  where
    walk :: Puzzle -> Maybe (Puzzle, Puzzle)
    walk (Puzzle walls limit guard) =
      let nextGuard = forward guard
       in if position nextGuard `elem` walls
            then walk (Puzzle walls limit (turn guard))
            else Just $ dupe (Puzzle walls limit nextGuard)

process1 :: Puzzle -> Int
process1 =
  length
    . nub
    . fmap (position . guard)
    . generation

run1 :: String -> String
run1 = show . process1 . read' readPrecPuzzle

candidates :: Puzzle -> [Puzzle]
candidates puzzle =
  ( \guard ->
      Puzzle
        (position guard `insert` walls puzzle)
        (limit puzzle)
        (backward guard)
  )
    <$> candidates
  where
    candidates =
      nubBy (\p1 p2 -> position p1 == position p2) $
        (guard puzzle :) $
          guard
            <$> generation puzzle

hasLoop :: [Puzzle] -> Bool
hasLoop = go empty . fmap guard
  where
    go :: Set Guard -> [Guard] -> Bool
    go _ [] = False
    go visited (x : xs) =
      x `elem` visited || go (x `insert` visited) xs

process2 :: Puzzle -> Int
process2 =
  length
    . filter id
    . parMap rpar (hasLoop . generation)
    . candidates

run2 :: String -> String
run2 = show . process2 . read' readPrecPuzzle
