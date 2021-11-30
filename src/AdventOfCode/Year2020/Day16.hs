module AdventOfCode.Year2020.Day16
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
import Data.Foldable.Extra (concat, concatMap, length, productOn')
import Data.Ix (Ix (inRange))
import Data.List (delete, foldl, isPrefixOf, length, nub, transpose, zip)
import Data.Maybe (mapMaybe)
import Debug.Trace
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

numbers :: Ticket -> [Int]
numbers (Ticket numbers) = numbers

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

anyP :: (Foldable t) => t (a -> Bool) -> a -> Bool
anyP = foldl (liftA2 (||)) (const False)

isValid :: [Rule] -> Int -> Bool
isValid =
  anyP
    . fmap inRange
    . concatMap
      (\(Rule _ ranges) -> ranges)

process1 :: Note -> Int
process1 (Note rules _ tickets) =
  sum $
    filter (not . isValid rules) $
      concatMap numbers tickets

run1 :: String -> String
run1 = show . process1 . (read :: String -> Note)

validRules :: [Rule] -> [Int] -> [Rule]
validRules rules xs = filter (\rule -> and $ isValid [rule] <$> xs) rules

possibilities :: [[String]] -> [String]
possibilities xs =
  if length solvedFields == length xs
    then fmap head xs
    else
      possibilities $
        fmap
          ( \fields ->
              if length fields == 1
                then fields
                else foldr delete fields solvedFields
          )
          xs
  where
    solvedFields = concat $ filter ((<= 1) . length) xs

-- possibilities = head . foldr go [[]] . traceShowId
--   where
--     go :: [String] -> [[String]] -> [[String]]
--     go xs acc =
--       concatMap
--         ( \x ->
--             mapMaybe
--               ( \tail ->
--                   if x `elem` tail
--                     then Nothing
--                     else return $ tail <> [x]
--               )
--               acc
--         )
--         xs

process2 :: Note -> Int
process2 (Note rules ticket tickets) =
  productOn' snd $
    filter (isPrefixOf "departure" . fst) $
      zip fields (numbers ticket)
  where
    fields :: [String]
    fields =
      possibilities $
        fmap
          (fmap (\(Rule title _) -> title) . validRules rules)
          ( transpose $
              filter (and . fmap (isValid rules)) $
                fmap numbers (ticket : tickets)
          )

run2 :: String -> String
run2 = show . process2 . (read :: String -> Note)
