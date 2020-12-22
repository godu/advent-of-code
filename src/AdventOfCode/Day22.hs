module AdventOfCode.Day22
  ( run1,
    run2,
  )
where

import Data.List (find, unfoldr)
import qualified Data.Set as S (Set, empty, insert)
import Debug.Trace (traceShowId)
import Text.ParserCombinators.ReadP (eof, sepBy, skipSpaces, string)
import Text.Read (Read (readPrec), ReadPrec, lift, minPrec, readPrec_to_P)

type Card = Int

newtype Player = Player [Card] deriving (Show, Ord, Eq)

instance Read Player where
  readPrec = do
    lift $ string "Player "
    (readPrec :: ReadPrec Int)
    lift $ string ":\n"
    cards <- lift $ sepBy (readPrec_to_P readPrec minPrec) (string "\n")
    return $ Player cards

data Game = Game Player Player deriving (Show, Ord, Eq)

instance Read Game where
  readPrec = do
    playerA <- readPrec
    lift $ string "\n\n"
    playerB <- readPrec
    lift skipSpaces
    lift eof
    return $ Game playerA playerB

deck :: Game -> [Card]
deck (Game (Player []) (Player bs)) = bs
deck (Game (Player as) _) = as

answer :: [Card] -> Int
answer = sum . fmap (uncurry (*)) . zip [1 ..] . reverse

process1 :: Game -> Int
process1 =
  maybe 0 (answer . deck)
    . find isEnd
    . rounds
  where
    isEnd :: Game -> Bool
    isEnd (Game (Player []) _) = True
    isEnd (Game _ (Player [])) = True
    isEnd _ = False

    rounds :: Game -> [Game]
    rounds = unfoldr go
      where
        go :: Game -> Maybe (Game, Game)
        go (Game (Player []) (Player bs)) = Nothing
        go (Game (Player as) (Player [])) = Nothing
        go (Game (Player (a : as)) (Player (b : bs))) =
          return $
            (\g -> (g, g)) $
              if a >= b
                then Game (Player (as <> [a, b])) (Player bs)
                else Game (Player as) (Player (bs <> [b, a]))

run1 :: String -> String
run1 = show . process1 . read

process2 :: Game -> Int
process2 = answer . deck . last . rounds
  where
    rounds :: Game -> [Game]
    rounds = takeUntilDoesntOccured . unfoldr go
      where
        takeUntilDoesntOccured :: Ord a => [a] -> [a]
        takeUntilDoesntOccured = go S.empty
          where
            go :: Ord a => S.Set a -> [a] -> [a]
            go _ [] = []
            go s (x : xs) =
              if x `elem` s
                then []
                else x : go (x `S.insert` s) xs
        -- go a

        go :: Game -> Maybe (Game, Game)
        go (Game (Player []) (Player bs)) = Nothing
        go (Game (Player as) (Player [])) = Nothing
        go (Game (Player (a : as)) (Player (b : bs)))
          | a <= length as && b <= length bs =
            return $
              (\g -> (g, g)) $
                if not . playerBWin $ last $ rounds (Game (Player $ take a as) (Player $ take b bs))
                  then Game (Player (as <> [a, b])) (Player bs)
                  else Game (Player as) (Player (bs <> [b, a]))
          | otherwise =
            return $
              (\g -> (g, g)) $
                if a >= b
                  then Game (Player (as <> [a, b])) (Player bs)
                  else Game (Player as) (Player (bs <> [b, a]))
          where
            playerBWin :: Game -> Bool
            playerBWin (Game (Player as) _) = null as

run2 :: String -> String
run2 = show . process2 . read
