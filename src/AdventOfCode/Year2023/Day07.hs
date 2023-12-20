module AdventOfCode.Year2023.Day07
  ( run1,
    run2,
  )
where

import AdventOfCode.Utils (many1, split)
import Data.List (group, groupBy, sort, sortBy, sortOn)
import Data.Ord (Down (Down), comparing)
import qualified Data.Ord
import GHC.Read (Read (readPrec))
import Text.ParserCombinators.ReadP (string)
import Text.ParserCombinators.ReadPrec (ReadPrec, get, lift)

newtype Input a = Input [(a, Int)] deriving (Show)

instance (Read a) => Read (Input a) where
  readPrec = do
    rounds <- split (lift $ string "\n") readPrecRound
    return $ Input rounds
    where
      readPrecRound :: (Read a) => ReadPrec (a, Int)
      readPrecRound = do
        cards <- readPrec
        lift $ string " "
        score <- readPrec
        return (cards, score)

data Card
  = Ace
  | King
  | Queen
  | Jack
  | Ten
  | Nine
  | Eight
  | Seven
  | Six
  | Five
  | Four
  | Three
  | Two
  deriving (Eq, Ord, Show, Enum)

instance Read Card where
  readPrec = do
    c <- get
    case c of
      'A' -> pure Ace
      'K' -> pure King
      'Q' -> pure Queen
      'J' -> pure Jack
      'T' -> pure Ten
      '9' -> pure Nine
      '8' -> pure Eight
      '7' -> pure Seven
      '6' -> pure Six
      '5' -> pure Five
      '4' -> pure Four
      '3' -> pure Three
      '2' -> pure Two
      _ -> fail "invalid card"

data HandType
  = FiveOfAKind
  | FourOfAKind
  | FullHouse
  | ThreeOfAKind
  | TwoPairs
  | OnePair
  | HighCard
  deriving (Eq, Ord, Show)

handType :: Hand -> HandType
handType (Hand cards) = case groups of
  [5] -> FiveOfAKind
  [1, 4] -> FourOfAKind
  [2, 3] -> FullHouse
  [1, 1, 3] -> ThreeOfAKind
  [1, 2, 2] -> TwoPairs
  [1, 1, 1, 2] -> OnePair
  _ -> HighCard
  where
    groups = sort $ map length $ group $ sort cards

newtype Hand = Hand [Card] deriving (Eq, Show)

cards :: Hand -> [Card]
cards (Hand cards) = cards

instance Ord Hand where
  compare a b = case compare (handType a) (handType b) of
    EQ -> compare (cards a) (cards b)
    x -> x

instance Read Hand where
  readPrec = do
    cards <- many1 readPrec
    return $ Hand cards

process :: (Ord a) => Input a -> Int
process (Input rounds) =
  sum $
    zipWith
      (*)
      [1 ..]
      (snd <$> sortOn (Down . fst) rounds)

run1 :: String -> String
run1 = show . (process :: Input Hand -> Int) . read

data Card'
  = Ace'
  | King'
  | Queen'
  | Ten'
  | Nine'
  | Eight'
  | Seven'
  | Six'
  | Five'
  | Four'
  | Three'
  | Two'
  | Jack'
  deriving (Eq, Ord, Show, Enum)

instance Read Card' where
  readPrec = do
    c <- get
    case c of
      'A' -> pure Ace'
      'K' -> pure King'
      'Q' -> pure Queen'
      'T' -> pure Ten'
      '9' -> pure Nine'
      '8' -> pure Eight'
      '7' -> pure Seven'
      '6' -> pure Six'
      '5' -> pure Five'
      '4' -> pure Four'
      '3' -> pure Three'
      '2' -> pure Two'
      'J' -> pure Jack'
      _ -> fail "invalid card"

data HandType'
  = FiveOfAKind'
  | FourOfAKind'
  | FullHouse'
  | ThreeOfAKind'
  | TwoPairs'
  | OnePair'
  | HighCard'
  deriving (Eq, Ord, Show)

handType' :: Hand' -> HandType'
handType' (Hand' cards) = case groups of
  [] -> FiveOfAKind'
  [5] -> FiveOfAKind'
  [4, 1] -> FourOfAKind'
  [3, 2] -> FullHouse'
  [3, 1, 1] -> ThreeOfAKind'
  [2, 2, 1] -> TwoPairs'
  [2, 1, 1, 1] -> OnePair'
  _ -> HighCard'
  where
    jokers = length $ filter (== Jack') cards
    groups = zipWith (+) (jokers : repeat 0) $ sortBy (comparing Down) (map length $ group $ sort $ filter (/= Jack') cards)

newtype Hand' = Hand' [Card'] deriving (Eq, Show)

cards' :: Hand' -> [Card']
cards' (Hand' cards) = cards

instance Ord Hand' where
  compare a b = case compare (handType' a) (handType' b) of
    EQ -> compare (cards' a) (cards' b)
    x -> x

instance Read Hand' where
  readPrec = do
    cards <- many1 readPrec
    return $ Hand' cards

run2 :: String -> String
run2 = show . (process :: Input Hand' -> Int) . read
