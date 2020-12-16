module AdventOfCode.Day16
  ( run1,
    process1,
    run2,
    process2,
    Note (Note),
    Rule (Rule),
    Ticket (Ticket),
  )
where

import Control.Applicative (Applicative (liftA2))
import Data.Ix (Ix (inRange))
import Text.ParserCombinators.ReadP (eof, munch1, sepBy, skipSpaces, string)
import Text.ParserCombinators.ReadPrec (lift, minPrec, pfail, readPrec_to_P)
import Text.Read (Read (readPrec), readPrec)

data Rule = Rule String [(Int, Int)] deriving (Show, Eq)

instance Read Rule where
  readPrec = do
    title <- lift $ munch1 (/= ':')
    lift $ string ": "
    a <- readPrec
    lift $ string "-"
    b <- readPrec
    lift $ string " or "
    c <- readPrec
    lift $ string "-"
    d <- readPrec
    return $ Rule title [(a, b), (c, d)]

newtype Ticket = Ticket [Int] deriving (Show, Eq)

instance Read Ticket where
  readPrec = do
    numbers <- lift $ sepBy (readPrec_to_P readPrec minPrec) (string ",")

    if null numbers
      then pfail
      else return $ Ticket numbers

data Note = Note [Rule] Ticket [Ticket] deriving (Show, Eq)

instance Read Note where
  readPrec = do
    rules <- lift $ sepBy (readPrec_to_P readPrec minPrec) (string "\n")
    lift skipSpaces

    lift $ string "your ticket:"
    lift skipSpaces
    yourTicket <- readPrec
    lift skipSpaces

    lift $ string "nearby tickets:"
    lift skipSpaces
    tickets <- lift $ sepBy (readPrec_to_P readPrec minPrec) (string "\n")

    lift skipSpaces

    lift eof

    return $ Note rules yourTicket tickets

process1 :: Note -> Int
process1 (Note rules _ tickets) =
  sum $
    filter (not . isValid) $
      concatMap
        (\(Ticket numbers) -> numbers)
        tickets
  where
    isValid =
      foldl (liftA2 (||)) (const False) $
        inRange
          <$> concatMap
            (\(Rule _ ranges) -> ranges)
            rules

run1 :: String -> String
run1 = show . process1 . (read :: String -> Note)

process2 :: Int -> Int
process2 = id

run2 :: String -> String
run2 = const ""
