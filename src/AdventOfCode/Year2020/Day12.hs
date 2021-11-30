module AdventOfCode.Year2020.Day12
  ( run1,
    process1,
    run2,
    process2,
  )
where

import Data.Bifunctor (Bifunctor (bimap))
import Text.ParserCombinators.ReadP (string)
import Text.ParserCombinators.ReadPrec (lift, (+++))
import Text.Read (Read (readPrec))

data Instruction
  = MoveNorth Int
  | MoveSouth Int
  | MoveEast Int
  | MoveWest Int
  | TurnLeft Int
  | TurnRight Int
  | Forward Int

instance Read Instruction where
  readPrec =
    ( do
        lift $ MoveNorth <$ string "N"
        MoveNorth <$> readPrec
    )
      +++ ( do
              lift $ MoveNorth <$ string "S"
              MoveSouth <$> readPrec
          )
      +++ ( do
              lift $ MoveNorth <$ string "E"
              MoveEast <$> readPrec
          )
      +++ ( do
              lift $ MoveNorth <$ string "W"
              MoveWest <$> readPrec
          )
      +++ ( do
              lift $ MoveNorth <$ string "L"
              TurnLeft <$> readPrec
          )
      +++ ( do
              lift $ MoveNorth <$ string "R"
              TurnRight <$> readPrec
          )
      +++ ( do
              lift $ MoveNorth <$ string "F"
              Forward <$> readPrec
          )

data Direction
  = North
  | South
  | East
  | West
  deriving (Show)

data State = State Direction (Int, Int) deriving (Show)

process1 :: [Instruction] -> Int
process1 =
  uncurry (+)
    . bimap abs abs
    . (\(State _ position) -> position)
    . foldl apply (State East (0, 0))
  where
    turnLeft :: Direction -> Direction
    turnLeft North = West
    turnLeft West = South
    turnLeft South = East
    turnLeft East = North

    turnRight :: Direction -> Direction
    turnRight North = East
    turnRight East = South
    turnRight South = West
    turnRight West = North

    apply :: State -> Instruction -> State
    apply (State direction (east, north)) (MoveNorth x) = State direction (east, north + x)
    apply (State direction (east, north)) (MoveSouth x) = State direction (east, north - x)
    apply (State direction (east, north)) (MoveEast x) = State direction (east + x, north)
    apply (State direction (east, north)) (MoveWest x) = State direction (east - x, north)
    apply state (TurnLeft 0) = state
    apply (State direction position) (TurnLeft x) = apply (State (turnLeft direction) position) (TurnLeft $ x - 90)
    apply state (TurnRight 0) = state
    apply (State direction position) (TurnRight x) = apply (State (turnRight direction) position) (TurnRight $ x - 90)
    apply (State North position) (Forward x) = apply (State North position) (MoveNorth x)
    apply (State East position) (Forward x) = apply (State East position) (MoveEast x)
    apply (State South position) (Forward x) = apply (State South position) (MoveSouth x)
    apply (State West position) (Forward x) = apply (State West position) (MoveWest x)

run1 :: String -> String
run1 = show . process1 . fmap read . lines

data StateWithWaypoint = StateWithWaypoint (Int, Int) (Int, Int) deriving (Show)

process2 :: [Instruction] -> Int
process2 = uncurry (+) . bimap abs abs . (\(StateWithWaypoint position _) -> position) . foldl apply (StateWithWaypoint (0, 0) (10, 1))
  where
    apply :: StateWithWaypoint -> Instruction -> StateWithWaypoint
    apply (StateWithWaypoint position (east, north)) (MoveNorth x) = StateWithWaypoint position (east, north + x)
    apply (StateWithWaypoint position (east, north)) (MoveSouth x) = StateWithWaypoint position (east, north - x)
    apply (StateWithWaypoint position (east, north)) (MoveEast x) = StateWithWaypoint position (east + x, north)
    apply (StateWithWaypoint position (east, north)) (MoveWest x) = StateWithWaypoint position (east - x, north)
    apply state (TurnLeft 0) = state
    apply (StateWithWaypoint position (east, north)) (TurnLeft x) = apply (StateWithWaypoint position (- north, east)) (TurnLeft $ x - 90)
    apply state (TurnRight 0) = state
    apply (StateWithWaypoint position (east, north)) (TurnRight x) = apply (StateWithWaypoint position (north, - east)) (TurnRight $ x - 90)
    apply (StateWithWaypoint (east, north) (dEast, dNorth)) (Forward x) = StateWithWaypoint (east + dEast * x, north + dNorth * x) (dEast, dNorth)

run2 :: String -> String
run2 = show . process2 . fmap read . lines
