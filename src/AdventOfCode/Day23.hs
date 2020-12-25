module AdventOfCode.Day23
  ( run1,
    process1,
    run2,
    process2,
    go,
  )
where

import AdventOfCode.Utils (singleton)
import Data.Ix (Ix (inRange))
import Data.List (intercalate, partition, unfoldr)
import Data.List.Extra (trim)
import Data.Maybe (fromJust)
import Data.Tuple (swap)
import qualified Data.Vector as V (Vector, break, concat, drop, dropWhile, findIndex, fromList, generate, head, length, singleton, slice, tail, take, toList, (!), (++), (//))

go :: Int -> Int -> V.Vector Int -> V.Vector Int
go 0 _ cups = cups
go move i cups =
  go (move - 1) ((i + 1) `mod` length cups) $
    ( V.//
        ( zip
            ((`mod` length cups) <$> [destinationCupIndex - 2 ..])
            (V.toList treeCups)
            <> ( shift 3 (i + 1) (destinationCupIndex - 3) $
                   cups
               )
        )
    )
      $ cups
  where
    currentCup = cups V.! i
    treeCups = take (i + 1) 3 cups

    destinationCup = minusOne (V.toList treeCups) (V.length cups) currentCup
    destinationCupIndex =
      fromJust $
        V.findIndex (== destinationCup) cups

    take :: Show a => Int -> Int -> V.Vector a -> V.Vector a
    take i n v = xs V.++ ys
      where
        l = V.length v
        n' = min (l - i) n
        xs = V.slice i n' v
        ys = V.slice 0 (n - n') v

    minusOne :: [Int] -> Int -> Int -> Int
    minusOne treeCups max currentCup =
      head $
        filter (`notElem` treeCups) $
          unfoldr
            ( \b ->
                return $
                  if b <= 1
                    then (max, max)
                    else (b -1, b -1)
            )
            currentCup

    shift :: Show a => Int -> Int -> Int -> V.Vector a -> [(Int, a)]
    shift move startIndex endIndex arr =
      zip
        ((`mod` l) <$> r)
        $ (arr V.!)
          . (`mod` l)
          . (+ move)
          <$> r
      where
        l = length arr
        r =
          if startIndex <= endIndex
            then [startIndex .. endIndex]
            else (`mod` l) <$> [startIndex .. endIndex + l]

process1 :: Int -> [Int] -> [Int]
process1 move xs =
  V.toList $
    answer $
      go move 0 $ V.fromList xs
  where
    answer =
      V.tail
        . uncurry (<>)
        . swap
        . V.break (== 1)

run1 :: String -> String
run1 =
  intercalate ""
    . fmap show
    . process1 100
    . fmap (read . singleton)
    . trim

process2 :: Int -> [Int] -> Int
process2 move xs =
  product $
    V.take 3 $
      V.dropWhile (/= 1) $
        go move 0 input
  where
    input = V.generate 1000000 (+ 1) V.// zip [0 ..] xs

run2 :: String -> String
run2 = const ""
