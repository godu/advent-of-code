module AdventOfCode.Year2020.Day15
  ( run1,
    run2,
  )
where

import Control.Monad.ST (ST, runST)
import Data.Foldable (foldlM)
import Data.List.Extra (splitOn)
import qualified Data.Vector.Unboxed.Mutable as V

solve :: Int -> [Int] -> Int
solve steps initial =
  runST $ do
    vector <- V.new steps

    mapM_ (uncurry $ V.write vector) $ zip (init initial) [1 ..]

    foldlM
      (step vector)
      (last initial)
      [length initial .. steps - 1]
  where
    step :: V.MVector s Int -> Int -> Int -> ST s Int
    step vector num counter = do
      current <- V.read vector num
      V.write vector num counter
      return $
        case current of
          0 -> 0
          lastCount -> counter - lastCount

run1 :: String -> String
run1 = show . solve 2020 . fmap read . splitOn ","

run2 :: String -> String
run2 = show . solve 30000000 . fmap read . splitOn ","
