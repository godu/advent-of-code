{-# LANGUAGE TupleSections #-}

module AdventOfCode.Year2020.Day20
  ( run1,
    process1,
    run2,
    process2,
    edges,
    findPlace,
    turn,
    vFlip,
    puzzleToList,
    markMonster,
    hFlip,
    combineTiles,
    resolve,
    Puzzle (Puzzle),
    Tile (Tile),
  )
where

import AdventOfCode.Utils (read', singleton)
import Data.Char (isLetter)
import Data.List (delete, find, partition, unfoldr)
import Data.Map as M (Map, empty, fromList, insert, keys, (!?))
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
import Data.Set as S (Set, difference, fromList, toList, unions)
import Text.ParserCombinators.ReadP (char, eof, munch1, sepBy, skipSpaces, string)
import Text.Read (ReadPrec, lift, minPrec, readListPrec, readPrec, readPrec_to_P)

data Tile = Tile Int [[Char]] deriving (Show)

instance Eq Tile where
  (Tile id _) == (Tile id' _) = id == id'

topHash :: Tile -> String
topHash (Tile _ pic) = head pic

leftHash :: Tile -> String
leftHash (Tile _ pic) = head <$> pic

bottomHash :: Tile -> String
bottomHash (Tile _ pic) = last pic

rightHash :: Tile -> String
rightHash (Tile _ pic) = last <$> pic

instance Read Tile where
  readPrec = do
    lift $ string "Tile "
    id <- readPrec
    lift $ string ":\n"
    xs <- lift $ sepBy (munch1 (`elem` ".#")) (char '\n')
    return $ Tile id $ xs

  readListPrec = do
    tiles <- lift $ sepBy (readPrec_to_P readPrec minPrec) (string "\n\n")
    lift skipSpaces
    lift eof
    return $ tiles

turn :: Tile -> Tile
turn (Tile id pic) =
  Tile id $
    foldl1 (zipWith (flip (<>))) $
      fmap (fmap singleton) pic

hFlip :: Tile -> Tile
hFlip (Tile id pic) =
  Tile id $ reverse pic

vFlip :: Tile -> Tile
vFlip (Tile id pic) =
  Tile id $ reverse <$> pic

arrangements :: Tile -> [Tile]
arrangements t =
  [ t,
    turn t,
    turn $ turn t,
    turn $ turn $ turn t,
    vFlip t,
    turn $ vFlip t,
    turn $ turn $ vFlip t,
    turn $ turn $ turn $ vFlip t
  ]

puzzleToList :: Puzzle -> [[Tile]]
puzzleToList (Puzzle puzzle) =
  ( \y ->
      mapMaybe
        (\x -> puzzle !? (x, y))
        [minX .. maxX]
  )
    <$> [minY .. maxY]
  where
    ((minX, minY), (maxX, maxY)) = bounds $ Puzzle puzzle

combineTiles :: [[Tile]] -> Tile
combineTiles =
  Tile 0
    . foldr
      ( (<>)
          . foldr (zipWith (flip (<>)) . fmap (init . tail) . init . tail . (\(Tile _ pic) -> pic)) (repeat [])
      )
      []

newtype Puzzle = Puzzle (Map (Int, Int) Tile) deriving (Show, Eq)

bounds :: Puzzle -> ((Int, Int), (Int, Int))
bounds (Puzzle puzzle) =
  ( foldl (append min) (0, 0) $ keys puzzle,
    foldl (append max) (0, 0) $ keys puzzle
  )
  where
    append f (x, y) (x', y') = (f x x', f y y')

isComplete :: Puzzle -> Bool
isComplete (Puzzle puzzle) =
  let ((minX, minY), (maxX, maxY)) = bounds (Puzzle puzzle)
   in ((maxX - minX + 1) * (maxY - minY + 1)) == length puzzle

edges :: Puzzle -> Set (Int, Int)
edges (Puzzle puzzle) = (`difference` allKeys) $ unions $ neighbors <$> S.toList allKeys
  where
    allKeys :: Set (Int, Int)
    allKeys = S.fromList $ keys puzzle
    neighbors :: (Int, Int) -> Set (Int, Int)
    neighbors (x, y) = S.fromList [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]

findPlace :: Puzzle -> [Tile] -> Maybe (Int, Int)
findPlace puzzle tiles =
  find
    (matchWith' tiles . compatibleHashes puzzle)
    $ S.toList allEdges
  where
    allEdges = edges puzzle
    matchWith' tiles hashes =
      any (any (matchWith hashes) . arrangements) tiles

compatibleHashes :: Puzzle -> (Int, Int) -> (Maybe String, Maybe String, Maybe String, Maybe String)
compatibleHashes (Puzzle puzzle) (x, y) =
  ( bottomHash <$> puzzle !? (x, y - 1),
    rightHash <$> puzzle !? (x + 1, y),
    topHash <$> puzzle !? (x, y + 1),
    leftHash <$> puzzle !? (x - 1, y)
  )

matchWith :: (Maybe String, Maybe String, Maybe String, Maybe String) -> Tile -> Bool
matchWith (mtop, mleft, mbottom, mrigth) tile =
  maybe True (== topHash tile) mtop
    && maybe True (== leftHash tile) mleft
    && maybe True (== bottomHash tile) mbottom
    && maybe True (== rightHash tile) mrigth

resolve :: [Tile] -> Maybe Puzzle
resolve (x : xs) = listToMaybe $ go (Puzzle (M.fromList [((0, 0), x)])) xs
  where
    go :: Puzzle -> [Tile] -> [Puzzle]
    go puzzle [] = [puzzle | isComplete puzzle]
    go (Puzzle puzzle) tiles =
      concatMap
        ( \(place, tile) ->
            go
              (Puzzle $ M.insert place tile puzzle)
              (tile `delete` tiles)
        )
        candidates
      where
        allArrangements = concatMap arrangements tiles
        candidates =
          concatMap
            (filter (matchWith'' $ Puzzle puzzle) . (\p -> (p,) <$> allArrangements))
            ( S.toList $
                edges $
                  Puzzle puzzle
            )
        matchWith'' puzzle (place, tile) = matchWith (compatibleHashes puzzle place) tile

process1 :: [Tile] -> Int
process1 = maybe 0 answers . resolve
  where
    answers (Puzzle puzzle) =
      product $
        maybe 0 id_ . (puzzle !?)
          <$> [ (minX, minY),
                (minX, maxY),
                (maxX, minY),
                (maxX, maxY)
              ]
      where
        ((minX, minY), (maxX, maxY)) = bounds $ Puzzle puzzle
        id_ (Tile i _) = i

run1 :: String -> String
run1 = show . process1 . read

markMonster :: [[Char]] -> [[Char]]
markMonster = eachCells markMonster'
  where
    eachCells :: ([[a]] -> [[a]]) -> [[a]] -> [[a]]
    eachCells = eachRows . eachColumns
    eachColumns :: ([[a]] -> [[a]]) -> [[a]] -> [[a]]
    eachColumns f arr =
      foldl
        ( \arr i ->
            let heads = take i <$> arr
                tails = drop i <$> arr
             in zipWith (<>) heads $ f tails
        )
        arr
        [0 .. l]
      where
        l = maximum $ length <$> arr
    eachRows :: ([[a]] -> [[a]]) -> [[a]] -> [[a]]
    eachRows f arr =
      foldl
        ( \arr i ->
            let (head, tail) = splitAt i arr
             in head <> f tail
        )
        arr
        [0 .. (length arr - 1)]
    markMonster' :: [[Char]] -> [[Char]]
    markMonster'
      ( (a00 : a01 : a02 : a03 : a04 : a05 : a06 : a07 : a08 : a09 : a10 : a11 : a12 : a13 : a14 : a15 : a16 : a17 : '#' : a19 : as)
          : ('#' : b01 : b02 : b03 : b04 : '#' : '#' : b07 : b08 : b09 : b10 : '#' : '#' : b13 : b14 : b15 : b16 : '#' : '#' : '#' : bs)
          : (c00 : '#' : c02 : c03 : '#' : c05 : c06 : '#' : c08 : c09 : '#' : c11 : c12 : '#' : c14 : c15 : '#' : c17 : c18 : c19 : cs)
          : xs
        ) =
        (a00 : a01 : a02 : a03 : a04 : a05 : a06 : a07 : a08 : a09 : a10 : a11 : a12 : a13 : a14 : a15 : a16 : a17 : 'O' : a19 : as)
          : ('O' : b01 : b02 : b03 : b04 : 'O' : 'O' : b07 : b08 : b09 : b10 : 'O' : 'O' : b13 : b14 : b15 : b16 : 'O' : 'O' : 'O' : bs)
          : (c00 : 'O' : c02 : c03 : 'O' : c05 : c06 : 'O' : c08 : c09 : 'O' : c11 : c12 : 'O' : c14 : c15 : 'O' : c17 : c18 : c19 : cs)
          : xs
    markMonster' xs = xs

process2 :: [Tile] -> Int
process2 tiles = fromMaybe 0 $ do
  (Puzzle puzzle) <- resolve tiles
  let bigPic = combineTiles $ puzzleToList $ Puzzle puzzle
  return $
    minimum $
      length
        . filter (== '#')
        . concat
        . markMonster
        . (\(Tile _ pic) -> pic)
        <$> arrangements bigPic

run2 :: String -> String
run2 = show . process2 . read
