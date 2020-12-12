module AdventOfCode.Day12
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

data State = State {direction :: Direction, position :: (Int, Int)} deriving (Show)

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

process1 :: [Instruction] -> Int
process1 = uncurry (+) . bimap abs abs . position . foldl apply (State East (0, 0))

run1 :: String -> String
run1 = show . process1 . fmap read . lines

process2 :: Int -> Int
process2 = id

run2 :: String -> String
run2 = const ""
