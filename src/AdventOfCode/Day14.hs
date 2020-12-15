{-# LANGUAGE TupleSections #-}

module AdventOfCode.Day14
  ( run1,
    process1,
    run2,
    process2,
  )
where

import Data.List (unfoldr)
import Data.Map (empty, insert)
import Data.Maybe (catMaybes)
import Text.ParserCombinators.ReadP (eof, many, string)
import Text.ParserCombinators.ReadPrec (ReadPrec, lift, minPrec, readPrec_to_P, (+++))
import Text.Read (Read (readPrec))

type Bits = [Bool]

type Mask = [Maybe Bool]

data Instruction
  = Mask Mask
  | Memory Int Int
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
              address <- readPrec
              lift $ string "] = "
              bits <- readPrec
              lift eof
              return $ Memory address bits
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

data State = State Mask [Instruction]

applyMask :: Mask -> Int -> Int
applyMask mask = bitsToInt . fmap applyMask' . zip mask . intToBits
  where
    applyMask' (Nothing, x) = x
    applyMask' (Just x, _) = x

process1 :: [Instruction] -> Int
process1 =
  sum
    . foldl (\memories (address, value) -> insert address value memories) empty
    . catMaybes
    . unfoldr go
    . State []
  where
    go :: State -> Maybe (Maybe (Int, Int), State)
    go (State _ []) = Nothing
    go (State _ (Mask mask : instructions)) = return (Nothing, State mask instructions)
    go (State mask (Memory address value : instructions)) =
      return
        ( Just (address, applyMask mask value),
          State mask instructions
        )

run1 :: String -> String
run1 = show . process1 . fmap (read :: String -> Instruction) . lines

toAddresses :: Int -> Mask -> [Int]
toAddresses address = fmap bitsToInt . maskToAddresses . fmap applyMask . zip (intToBits address)
  where
    applyMask :: (Bool, Maybe Bool) -> Maybe Bool
    applyMask (x, Nothing) = Nothing
    applyMask (x, Just False) = Just x
    applyMask (_, Just True) = Just True

    maskToAddresses :: Mask -> [Bits]
    maskToAddresses (Nothing : xs) = ((False :) <$> maskToAddresses xs) <> ((True :) <$> maskToAddresses xs)
    maskToAddresses (Just x : xs) = (x :) <$> maskToAddresses xs
    maskToAddresses [] = [[]]

process2 :: [Instruction] -> Int
process2 =
  sum
    . foldl (\memories (address, value) -> insert address value memories) empty
    . concat
    . unfoldr go
    . State []
  where
    go :: State -> Maybe ([(Int, Int)], State)
    go (State _ []) = Nothing
    go (State _ (Mask mask : instructions)) = return ([], State mask instructions)
    go (State mask (Memory address value : instructions)) =
      return
        ( (,value)
            <$> toAddresses address mask,
          State mask instructions
        )

run2 :: String -> String
run2 = show . process2 . fmap (read :: String -> Instruction) . lines
