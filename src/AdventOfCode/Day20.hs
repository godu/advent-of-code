module AdventOfCode.Day20
  ( run1,
    process1,
    run2,
    process2,
    edges,
    findPlace,
    turn,
    vFlip,
    hFlip,
    Puzzle (Puzzle),
    Tile (Tile),
  )
where

import AdventOfCode.Utils (read')
import Data.Char (isLetter)
import Data.List (delete, find, partition, unfoldr)
import Data.Map as M (Map, empty, fromList, insert, keys, (!?))
import Data.Maybe (fromMaybe)
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

singleton :: a -> [a]
singleton a = [a]

turn :: Tile -> Tile
turn (Tile id pic) =
  Tile id $
    foldl1 (zipWith (flip (<>))) $
      fmap (fmap singleton) pic

vFlip :: Tile -> Tile
vFlip (Tile id pic) =
  Tile id $ reverse pic

hFlip :: Tile -> Tile
hFlip (Tile id pic) =
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

newtype Puzzle = Puzzle (Map (Int, Int) Tile) deriving (Show)

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
    neighbors (x, y) = S.fromList [(x + 1, y), (x -1, y), (x, y + 1), (x, y -1)]

findPlace :: Puzzle -> [Tile] -> Maybe (Int, Int)
findPlace puzzle tiles =
  find
    (matchWith' tiles . compatibleHashes puzzle)
    $ S.toList allEdges
  where
    allEdges = edges puzzle
    matchWith' tiles hashes =
      any (matchWith hashes) $
        concatMap
          arrangements
          tiles

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

process1 :: [Tile] -> Int
process1 (x : xs) = answers $ head $ go (Puzzle (M.fromList [((0, 0), x)])) xs
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
    go :: Puzzle -> [Tile] -> [Puzzle]
    go puzzle [] = [puzzle | isComplete puzzle]
    go puzzle tiles = fromMaybe [] $ do
      place <- findPlace puzzle tiles
      let (candidates, restTiles) =
            partition
              (any (matchWith (compatibleHashes puzzle place)) . arrangements)
              tiles

      return $
        concatMap
          ( \tile ->
              let Just rightArrangement =
                    find
                      (matchWith (compatibleHashes puzzle place))
                      $ arrangements tile
                  Puzzle puzzle' = puzzle
               in go
                    (Puzzle $ M.insert place rightArrangement puzzle')
                    (tile `delete` candidates <> restTiles)
          )
          candidates

run1 :: String -> String
run1 = show . process1 . read

process2 :: Int -> Int
process2 = id

run2 :: String -> String
run2 = const ""
