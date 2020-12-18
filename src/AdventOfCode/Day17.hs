module AdventOfCode.Day17
  ( run1,
    run2,
    neighbors,
    Coordinate (Coordinate),
  )
where

import Data.Ix (Ix (inRange))
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Set (Set, fromList, member, toList)
import Data.Tuple.Extra (uncurry3)
import Debug.Trace
import Text.ParserCombinators.ReadP (string)
import Text.Read (Read (readPrec), lift, (+++))

data State = Active | Inactive deriving (Eq)

instance Show State where
  show Active = "#"
  show Inactive = "."

instance Read State where
  readPrec =
    (Active <$ lift (string "#"))
      +++ (Inactive <$ lift (string "."))

data Coordinate = Coordinate Int Int Int deriving (Show, Eq, Ord)

newtype Grid = Grid (Set Coordinate) deriving (Show, Eq)

range :: (Coordinate, Coordinate) -> [Coordinate]
range (Coordinate x y z, Coordinate x' y' z') =
  (\[x, y, z] -> Coordinate x y z)
    <$> sequence
      [ [x .. x'],
        [y .. y'],
        [z .. z']
      ]

neighbors :: Coordinate -> [Coordinate]
neighbors (Coordinate x y z) =
  filter
    (/= Coordinate x y z)
    $ range
      ( Coordinate (pred x) (pred y) (pred z),
        Coordinate (succ x) (succ y) (succ z)
      )

bounds :: [Coordinate] -> (Coordinate, Coordinate)
bounds xs =
  ( foldl (append min) (Coordinate 0 0 0) xs,
    foldl (append max) (Coordinate 0 0 0) xs
  )
  where
    append f (Coordinate x y z) (Coordinate x' y' z') = Coordinate (f x x') (f y y') (f z z')

singleton :: a -> [a]
singleton x = [x]

generation :: Grid -> Grid
generation (Grid grid) =
  Grid $
    fromList $
      filter willBeAlive $
        range $
          ( \(Coordinate x y z, Coordinate x' y' z') ->
              ( Coordinate (pred x) (pred y) (pred z),
                Coordinate (succ x') (succ y') (succ z')
              )
          )
            $ bounds $
              toList grid
  where
    willBeAlive :: Coordinate -> Bool
    willBeAlive coord =
      if coord `member` grid
        then inRange (2, 3) $ length activeNeighbors
        else length activeNeighbors == 3
      where
        activeNeighbors = filter (`member` grid) $ neighbors coord

process1 :: Grid -> Int
process1 =
  length
    . toList
    . (\(Grid grid) -> grid)
    . generation
    . generation
    . generation
    . generation
    . generation
    . generation

run1 :: String -> String
run1 =
  show
    . process1
    . Grid
    . fromList
    . fmap fst
    . filter ((== Active) . snd)
    . fmap (\(x, y, s) -> (Coordinate x y 0, s))
    . concatMap (\(x, xs) -> (\(y, s) -> (x, y, s)) <$> xs)
    . zip [0 ..]
    . fmap
      ( zip [0 ..]
          . fmap
            ( (read :: String -> State) . singleton
            )
      )
    . lines

run2 :: String -> String
run2 = const ""
