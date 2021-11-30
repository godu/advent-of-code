module AdventOfCode.Year2020.Day25
  ( run1,
    process1,
  )
where

import Data.List (elemIndex)
import Data.Maybe (fromJust)

transform :: Int -> [Int]
transform sn = transform' sn 1
  where
    transform' sn v = v : transform' sn v'
      where
        v' = (v * sn) `rem` 20201227

process1 :: (Int, Int) -> Int
process1 (cardPublicKey, doorPublicKey) = encryptionKey
  where
    cardLoopSize = elemIndex cardPublicKey $ transform 7
    doorLoopSize = elemIndex doorPublicKey $ transform 7

    encryptionKey = transform cardPublicKey !! fromJust doorLoopSize

run1 :: String -> String
run1 =
  show
    . process1
    . (\[a, b] -> (a, b))
    . fmap read
    . lines
