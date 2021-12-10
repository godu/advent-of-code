module AdventOfCode.Year2021.Day04
  ( run1,
    process1,
    run2,
    process2,
  )
where

import Control.Applicative (Alternative (empty), optional)
import Data.Bifunctor (bimap)
import Data.Char (isDigit)
import Data.List (find, inits, transpose, uncons, (\\))
import Data.List.Extra (firstJust)
import Data.Maybe (fromMaybe)
import qualified Data.Set as S (Set, empty, insert)
import Text.ParserCombinators.ReadP (ReadP, char, many, munch1, sepBy1, string)
import Text.Read (Read (readPrec), lift, minPrec, readPrec_to_P)

readPInt :: ReadP Int
readPInt = read <$> munch1 isDigit

type Draw = [Int]

type Board = [[Int]]

data Game = Game Draw [Board] deriving (Show)

instance Read Game where
  readPrec = do
    draw <- lift $ sepBy1 readPInt (char ',')
    lift $ string "\n\n"
    boards <-
      lift $
        sepBy1
          ( sepBy1
              ( optional (char ' ')
                  *> sepBy1
                    readPInt
                    (many $ char ' ')
              )
              (char '\n')
          )
          (string "\n\n")
    return $ Game draw boards

isWin :: Draw -> Board -> Bool
isWin xs board = any (\row -> null $ row \\ xs) $ board ++ transpose board

pairs :: [a] -> [(a, a)]
pairs xs = zip (drop 1 xs) xs

unfoldGame :: Game -> [(Int, [Int], [Board])]
unfoldGame (Game draw boards) =
  zip3 draw (inits draw) $
    map (uncurry (\\)) $
      pairs $
        scanr (filter . isWin) boards $
          inits draw

solve :: (Int, [Int], [Board]) -> Int
solve (n, draw, boards) = sum (concat (concat boards) \\ (n : draw)) * n

process1 :: Game -> Int
process1 =
  solve
    . head
    . filter (\(_, _, boards) -> not $ null boards)
    . unfoldGame

run1 :: String -> String
run1 = show . process1 . read

process2 :: Game -> Int
process2 =
  solve
    . last
    . filter (\(_, _, boards) -> not $ null boards)
    . unfoldGame

run2 :: String -> String
run2 = show . process2 . read
