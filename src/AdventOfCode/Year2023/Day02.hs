module AdventOfCode.Year2023.Day02
  ( run1,
    run2,
  )
where

import Data.Map as M (Map, fromList, lookup)
import Data.Maybe (fromMaybe)
import Debug.Trace (traceShowId)
import Text.ParserCombinators.ReadP (choice, sepBy1, string)
import Text.ParserCombinators.ReadPrec (ReadPrec, lift, readPrec_to_P)
import Text.Read (Read (readPrec))

data Color = Red | Green | Blue deriving (Show, Eq, Ord)

instance Read Color where
  readPrec =
    lift $
      choice
        [ string "red" >> return Red,
          string "green" >> return Green,
          string "blue" >> return Blue
        ]

data Bag = Bag Int Int Int deriving (Show)

data Draw = Draw Int Int Int deriving (Show)

instance Read Draw where
  readPrec = do
    dices <-
      lift $
        sepBy1
          ( do
              count <- readPrec_to_P readPrec 1
              string " "
              color <- readPrec_to_P readPrec 1
              return $ (color, count)
          )
          (string ", ")
    let draw = M.fromList dices
    return $ Draw (pick draw Red) (pick draw Green) (pick draw Blue)
    where
      pick :: Map Color Int -> Color -> Int
      pick draw color = fromMaybe 0 $ M.lookup color draw

data Game = Game Int [Draw] deriving (Show)

instance Read Game where
  readPrec = do
    lift $ string "Game "
    i <- readPrec
    lift $ string ": "
    draws <-
      lift $
        sepBy1
          (readPrec_to_P readPrec 1)
          (string "; ")
    return $ Game i draws

isPossibleWith :: Bag -> Draw -> Bool
isPossibleWith (Bag r1 g1 b1) (Draw r2 g2 b2) =
  r1 >= r2 && g1 >= g2 && b1 >= b2

process1 :: [Game] -> Int
process1 =
  sum
    . map (\(Game id _) -> id)
    . filter (\(Game _ draws) -> (bag `isPossibleWith`) `all` draws)
  where
    bag = Bag 12 13 14

run1 :: String -> String
run1 =
  show
    . process1
    . fmap read
    . lines

process2 :: [Game] -> Int
process2 = sum . map (computePower . getMinimalBag)
  where
    computePower :: Bag -> Int
    computePower (Bag r g b) = r * g * b
    getMinimalBag :: Game -> Bag
    getMinimalBag (Game _ draws) = foldr (mergeBag . toBag) (Bag 0 0 0) draws
      where
        toBag :: Draw -> Bag
        toBag (Draw r g b) = Bag r g b
        mergeBag :: Bag -> Bag -> Bag
        mergeBag (Bag r1 g1 b1) (Bag r2 g2 b2) = Bag (max r1 r2) (max g1 g2) (max b1 b2)

run2 :: String -> String
run2 =
  show
    . process2
    . fmap read
    . lines
