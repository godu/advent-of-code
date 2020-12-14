module AdventOfCode.Day14
  ( run1,
    process1,
    run2,
    process2,
  )
where

import Data.Map (Map, empty, insert)
import Text.ParserCombinators.ReadP (eof, many, string)
import Text.ParserCombinators.ReadPrec (ReadPrec, lift, minPrec, readPrec_to_P, (+++))
import Text.Read (Read (readPrec))

type Bits = [Bool]

type Mask = [Maybe Bool]

data Instruction
  = Mask Mask
  | Memory Int Bits
  deriving (Show)

instance Read Instruction where
  readPrec =
    ( do
        lift $ string "mask = "
        bits <- lift $ many $ readPrec_to_P readPrecMaybeBool minPrec
        lift eof
        return $ Mask bits
    )
      +++ ( do
              lift $ string "mem["
              index <- readPrec
              lift $ string "] = "
              bits <- intToBits <$> readPrec
              lift eof
              return $ Memory index bits
          )
    where
      readPrecBool :: ReadPrec Bool
      readPrecBool =
        ( do
            lift $ string "0"
            return False
        )
          +++ do
            lift $ string "1"
            return True
      readPrecMaybeBool :: ReadPrec (Maybe Bool)
      readPrecMaybeBool =
        ( do
            lift $ string "X"
            return Nothing
        )
          +++ (return <$> readPrecBool)

intToBits :: Int -> Bits
intToBits x = reverse $ take 36 $ (++ repeat False) $ reverse $ intToBits' x []
  where
    intToBits' :: Int -> Bits -> Bits
    intToBits' 0 xs = xs
    intToBits' x xs =
      let (y, z) = x `divMod` 2
       in intToBits' y ((z == 1) : xs)

bitsToInt :: Bits -> Int
bitsToInt = bitsToInt' 0
  where
    bitsToInt' acc [] = acc
    bitsToInt' acc (x : xs) =
      bitsToInt'
        (acc * 2 + (if x then 1 else 0))
        xs

data State = State Mask (Map Int Bits)

initialState :: State
initialState = State (replicate 36 Nothing) empty

process1 :: [Instruction] -> Int
process1 =
  foldl ((. bitsToInt) . (+)) 0
    . (\(State _ memories) -> memories)
    . foldl go initialState
  where
    go :: State -> Instruction -> State
    go (State _ memories) (Mask mask) = State mask memories
    go (State mask memories) (Memory index value) = State mask $ insert index (applyMask mask value) memories

    applyMask :: Mask -> Bits -> Bits
    applyMask mask = fmap applyMask' . zip mask
      where
        applyMask' (Nothing, x) = x
        applyMask' (Just x, _) = x

run1 :: String -> String
run1 = show . process1 . fmap (read :: String -> Instruction) . lines

process2 :: Int -> Int
process2 = id

run2 :: String -> String
run2 = const ""
