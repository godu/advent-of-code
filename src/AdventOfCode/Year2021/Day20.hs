{-# LANGUAGE TupleSections #-}

module AdventOfCode.Year2021.Day20
  ( run1,
    run2,
  )
where

import AdventOfCode.Utils (many, many1, nTimes, split)
import AdventOfCode.Year2020.Day17 ()
import Control.Comonad (Comonad, extend, extract)
import Data.Bifunctor (first, second)
import Data.Function.Memoize (memoize)
import Data.Map as M (Map, empty, filter, fromList, keys, size, (!?))
import Data.Maybe (fromJust, fromMaybe)
import GHC.Arr (Array, listArray, (!))
import Text.ParserCombinators.ReadP (string)
import Text.Read (get, lift, readPrec, (<++))

data Pixel = Light | Dark deriving (Eq)

instance Enum Pixel where
  toEnum 0 = Dark
  toEnum _ = Light
  fromEnum Light = 1
  fromEnum Dark = 0

instance Show Pixel where
  show Light = "#"
  show Dark = "."

instance Read Pixel where
  readPrec =
    ( do
        '.' <- get
        return Dark
    )
      <++ ( do
              '#' <- get
              return Light
          )

type Point = (Int, Int)

data Picture a = Picture {focus :: Point, range :: (Point, Point), world :: Point -> a}

instance (Show a, Enum a) => Show (Picture a) where
  show (Picture i ((top, bottom), (left, right)) w) =
    unlines $
      ( \y ->
          ( \x ->
              show $ w (x, y)
          )
            `concatMap` [left .. right]
      )
        <$> [top .. bottom]

instance (Read a, Enum a) => Read (Picture a) where
  readPrec =
    do
      pixels <- split (lift $ string "\n") (many1 readPrec)
      let pixelsWithPoint = fromList $ (\(y, ps) -> first (,y) <$> zip [0 ..] ps) `concatMap` zip [0 ..] pixels
      let ((top, bottom), (left, right)) =
            foldr
              (\(x, y) ((top, bottom), (left, right)) -> ((min x top, max x bottom), (min y left, max y right)))
              ((0, 0), (0, 0))
              $ keys pixelsWithPoint
      return $
        Picture
          (0, 0)
          ((top - 1, bottom + 1), (left - 1, right + 1))
          (memoize (\p -> fromMaybe (toEnum 0) $ pixelsWithPoint !? p))

instance Functor Picture where
  f `fmap` (Picture i r w) = Picture i r $ f . w

instance Comonad Picture where
  extract (Picture i _ w) = w i
  extend f (Picture i r wa) = Picture i r $ memoize (\p -> f $ Picture p r wa)

neighbors :: Point -> (Point, Point, Point, Point, Point, Point, Point, Point, Point)
neighbors (x, y) =
  ( (x - 1, y - 1),
    (x, y - 1),
    (x + 1, y - 1),
    (x - 1, y),
    (x, y),
    (x + 1, y),
    (x - 1, y + 1),
    (x, y + 1),
    (x + 1, y + 1)
  )

newtype EnhancementAlgorithm = EnhancementAlgorithm (Array Int Pixel) deriving (Show)

instance Read EnhancementAlgorithm where
  readPrec = do
    pixels <- concat <$> split (lift $ string "\n") (many1 readPrec)
    return $
      EnhancementAlgorithm $
        listArray (0, 511) pixels

binaryToInt :: Enum a => [a] -> Int
binaryToInt bits =
  fst $
    foldr
      (\b (v, e) -> (if fromEnum b == 0 then v else v + 2 ^ e, e + 1))
      (0, 0)
      bits

calculatePixel :: EnhancementAlgorithm -> (Pixel, Pixel, Pixel, Pixel, Pixel, Pixel, Pixel, Pixel, Pixel) -> Pixel
calculatePixel (EnhancementAlgorithm m) (a, b, c, d, e, f, g, h, i) = m ! j
  where
    j = binaryToInt [a, b, c, d, e, f, g, h, i]

data Input = Input EnhancementAlgorithm (Picture Pixel) deriving (Show)

instance Read Input where
  readPrec = do
    algo <- readPrec
    '\n' <- get
    '\n' <- get
    Input algo <$> readPrec

enhance :: Int -> EnhancementAlgorithm -> Picture Pixel -> Picture Pixel
enhance n algo = nTimes n (nextStep algo)
  where
    nextStep :: EnhancementAlgorithm -> Picture Pixel -> Picture Pixel
    nextStep algo = zoomOut . extend (compute algo)
    zoomOut :: Picture Pixel -> Picture Pixel
    zoomOut (Picture i ((top, bottom), (left, right)) w) =
      Picture i ((top - 1, bottom + 1), (left - 1, right + 1)) w
    compute :: EnhancementAlgorithm -> Picture Pixel -> Pixel
    compute algo (Picture i r w) =
      calculatePixel algo $
        ( \(a, b, c, d, e, f, g, h, i) ->
            (w a, w b, w c, w d, w e, w f, w g, w h, w i)
        )
          $ neighbors i

render :: Show a => Picture a -> Map (Int, Int) a
render (Picture _ ((top, bottom), (left, right)) w) = fromList $ (\p -> (p, w p)) <$> points
  where
    points = (\x -> (x,) <$> [top .. bottom]) `concatMap` [left .. right]

process1 :: Input -> Int
process1 (Input algo picture) = size $ M.filter (== Light) $ render $ enhance 2 algo picture

run1 :: String -> String
run1 = show . process1 . read

process2 :: Input -> Int
process2 (Input algo picture) = size $ M.filter (== Light) $ render $ enhance 50 algo picture

run2 :: String -> String
run2 = show . process2 . read
