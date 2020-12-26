module AdventOfCode.Day23
  ( run1,
    run2,
    process1,
    process2,
  )
where

import AdventOfCode.Utils (singleton)
import Control.Monad (foldM_, zipWithM_)
import Control.Monad.Loops (unfoldrM)
import Control.Monad.ST (runST)
import Data.Ix (Ix (inRange))
import Data.List (intercalate, partition, unfoldr)
import Data.List.Extra (trim)
import Data.Maybe (fromJust, fromMaybe)
import Data.Tuple (swap)
import qualified Data.Vector.Mutable as V
import Debug.Trace

go v current = do
  x1 <- V.read v current
  x2 <- V.read v x1
  x3 <- V.read v x2
  next <- V.read v x3

  let l = V.length v
      x =
        head $
          filter (`notElem` [x1, x2, x3]) $
            unfoldr (\b -> return $ if b > 0 then (b - 1, b - 1) else (l - 1, l - 1)) current

  V.write v current next

  V.read v x >>= V.write v x3
  V.write v x x1

  return next

process1 n xs =
  fmap (+ 1) $
    runST $ do
      v <- V.new $ length xs'
      zipWithM_ (V.write v) xs' (tail xs' ++ xs')

      foldM_ (const . go v) (head xs') [1 .. n]

      unfoldrM
        ( \b -> do
            a <- V.read v b
            return $
              if a == 0
                then Nothing
                else Just (a, a)
        )
        0
  where
    xs' = (+ (-1)) <$> xs

run1 :: String -> String
run1 =
  intercalate ""
    . fmap show
    . process1 100
    . fmap (read . singleton)
    . trim

process2 n xs =
  runST $ do
    v <- V.new $ length xs'
    zipWithM_ (V.write v) xs' (tail xs' ++ xs')

    foldM_ (const . go v) (head xs') [1 .. n]

    x1 <- V.read v 0
    x2 <- V.read v x1

    return $ (x1 + 1) * (x2 + 1)
  where
    xs' = ((+ (-1)) <$> xs) ++ [length xs .. (1000000 - 1)]

run2 :: String -> String
run2 =
  show
    . process2 10000000
    . fmap (read . singleton)
    . trim
