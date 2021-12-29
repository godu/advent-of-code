{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module AdventOfCode.Year2021.Day23
  ( run1,
    run2,
  )
where

import AdventOfCode.Utils (many1)
import Algorithm.Search (dijkstra)
import Debug.Trace (traceShowId)
import Text.ParserCombinators.ReadP (string)
import Text.Read (get, lift, readPrec, (+++))

type Room = String

type Rooms = [Room]

type Hall = String

type Zustand = (Rooms, Hall)

type Amphibot = Char

type Cost = Int

pathCost :: [[Cost]]
pathCost =
  [ [3, 2, 2, 4, 6, 8, 9],
    [5, 4, 2, 2, 4, 6, 7],
    [7, 6, 4, 2, 2, 4, 5],
    [9, 8, 6, 4, 2, 2, 3]
  ]

roomFor :: Char -> Int
roomFor 'A' = 0
roomFor 'B' = 1
roomFor 'C' = 2
roomFor 'D' = 3
roomFor _ = error "Unknown room"

reachable :: Hall -> Int -> Int -> Bool
reachable hall r h
  | r > 3 || h > 6 = error ("Jumped out of Room or Hall " ++ show r ++ "# " ++ show h)
  | r + 1 >= h = all (\i -> hall !! i == '.') [h .. r + 1]
  | otherwise = all (\i -> hall !! i == '.') [r + 2 .. h]

replace :: [a] -> Int -> a -> [a]
replace xs i c = [if j == i then c else x | (j, x) <- zip [0 ..] xs]

getRoom :: Rooms -> Int -> Room
getRoom rooms i
  | i > 3 = error "XXX"
  | otherwise = rooms !! i

canMoveHome :: Room -> Char -> Bool
canMoveHome r a = all (== a) r

canReachHome :: Hall -> Int -> Int -> Bool
canReachHome hall h r
  | r > 3 || h > 6 = error ("Jumped out of Room or Hall " ++ show r ++ "# " ++ show h)
  | r + 1 >= h + 1 = all (\i -> hall !! i == '.') [h + 1 .. r + 1]
  | h - 1 >= r + 2 = all (\i -> hall !! i == '.') [r + 2 .. h -1]
  | otherwise = True

neighbors :: Zustand -> [Zustand]
neighbors (rooms, hall) = intoHall ++ outOfHall
  where
    intoHall =
      [ (rs', h')
        | i <- [0 .. 3],
          let room = getRoom rooms i,
          not (null room), -- Raum schon leer
          room /= replicate (length room) ("ABCD" !! i), -- Raum schon richtig belegt
          let (a : as) = room,
          j <- [0 .. 6],
          reachable hall i j, -- ist vom Raum der Punkt in der Halle erreichbar?
          let h' = replace hall j a,
          let rs' = [if k == i then as else getRoom rooms k | k <- [0 .. 3]]
      ]
    outOfHall =
      [ (rs', h')
        | i <- [0 .. 6],
          let a = hall !! i,
          a /= '.',
          let h' = replace hall i '.',
          let j = roomFor a,
          let r'' = rooms !! j,
          canMoveHome r'' a,
          canReachHome hall i j,
          let r' = a : r'',
          let rs' = replace rooms j r'
      ]

moveCost :: Char -> Int
moveCost 'A' = 1
moveCost 'B' = 10
moveCost 'C' = 100
moveCost 'D' = 1000
moveCost _ = error "Unknown amphibot in moveCost"

-- | Kosten ermittelt anhand Zustand **vor** Transition
costMove :: Int -> Zustand -> Int -> Int -> Cost
costMove len (rooms, hall) roomNr hallNr = (pathCost !! roomNr !! hallNr + offset) * moveCost a
  where
    as = rooms !! roomNr
    a = if hall !! hallNr /= '.' then hall !! hallNr else head as
    offset = if hall !! hallNr /= '.' then len - 1 - length as else len - length as

costTransition :: Int -> Zustand -> Zustand -> Cost
costTransition len (rs, h) (rs', h') = costMove len (rs, h) roomNr hallNr
  where
    roomNr = firstDifference rs rs'
    hallNr = firstDifference h h'

firstDifference :: Eq a => [a] -> [a] -> Int
firstDifference xs ys = head [i | i <- [0 .. length xs], xs !! i /= ys !! i]

solutionFound :: Int -> Zustand -> Bool
solutionFound len zustand = fst zustand == map (replicate len) "ABCD"

solve :: Rooms -> Maybe (Cost, [Zustand])
solve rooms = dijkstra neighbors (costTransition l) (solutionFound l) (rooms, ".......")
  where
    l = length $ head rooms

process1 :: Rooms -> Int
process1 rooms = maybe 0 fst $ solve rooms

run1 :: String -> String
run1 = show . process1 . lines

run2 :: String -> String
run2 = show . process1 . append . lines
  where
    append [a : as, b : bs, c : cs, d : ds] =
      [a : 'D' : 'D' : as, b : 'C' : 'B' : bs, c : 'B' : 'A' : cs, d : 'A' : 'C' : ds]
