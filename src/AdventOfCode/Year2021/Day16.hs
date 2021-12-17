module AdventOfCode.Year2021.Day16
  ( run1,
    run2,
  )
where

import AdventOfCode.Utils (read', singleton)
import Data.List.Extra (trim)
import Text.ParserCombinators.ReadPrec (get, (+++))
import Text.Read (ReadPrec, readPrec, (<++))

hexToBit :: Char -> String
hexToBit '0' = "0000"
hexToBit '1' = "0001"
hexToBit '2' = "0010"
hexToBit '3' = "0011"
hexToBit '4' = "0100"
hexToBit '5' = "0101"
hexToBit '6' = "0110"
hexToBit '7' = "0111"
hexToBit '8' = "1000"
hexToBit '9' = "1001"
hexToBit 'A' = "1010"
hexToBit 'B' = "1011"
hexToBit 'C' = "1100"
hexToBit 'D' = "1101"
hexToBit 'E' = "1110"
hexToBit 'F' = "1111"
hexToBit x = error $ "Not hex : " <> show x

many :: Show a => ReadPrec a -> ReadPrec [a]
many r =
  ( do
      a <- r
      as <- many r
      return (a : as)
  )
    <++ return []

times :: Int -> ReadPrec a -> ReadPrec [a]
times 0 r = return []
times n r = do
  a <- r
  as <- times (n - 1) r
  return (a : as)

data Packet
  = LiteralPacket {version :: Int, value :: Int}
  | OperatorPacket {version :: Int, identifier :: Int, packets :: [Packet]}
  deriving (Show)

bitToInt :: String -> Int
bitToInt =
  sum
    . fmap fst
    . filter ((== '1') . snd)
    . zip ((2 ^) <$> [0 ..])
    . reverse

instance Read Packet where
  readPrec =
    ( do
        version <- readPrecVersion
        4 <- readPrecIdentifier
        value <- bitToInt <$> readPrecValues
        return $ LiteralPacket version value
    )
      <++ ( do
              version <- readPrecVersion
              identifier <- readPrecIdentifier
              '0' <- get
              count <- bitToInt <$> times 15 get
              sub <- times count get
              let packets = read' (many readPrec) sub
              return $ OperatorPacket version identifier packets
          )
      <++ ( do
              version <- readPrecVersion
              identifier <- readPrecIdentifier
              '1' <- get
              count <- bitToInt <$> times 11 get
              packets <- times count readPrec
              return $ OperatorPacket version identifier packets
          )
    where
      readPrecVersion :: ReadPrec Int
      readPrecVersion = do
        a <- get
        b <- get
        c <- get
        return $ bitToInt [a, b, c]
      readPrecIdentifier :: ReadPrec Int
      readPrecIdentifier = readPrecVersion
      readPrecValues :: ReadPrec [Char]
      readPrecValues =
        ( do
            '1' <- get
            a <- get
            b <- get
            c <- get
            d <- get
            xs <- readPrecValues
            return $ ([a, b, c, d] <> xs)
        )
          <++ ( do
                  '0' <- get
                  a <- get
                  b <- get
                  c <- get
                  d <- get
                  return $ [a, b, c, d]
              )

newtype Input = Input [Packet] deriving (Show)

instance Read Input where
  readPrec = do
    packets <- many readPrec
    many
      ( do
          '0' <- get
          return ()
      )
    return $ Input packets

process1 :: Input -> Int
process1 (Input packets) = sumVersion packets
  where
    sumVersion [] = 0
    sumVersion (LiteralPacket version _ : xs) = version + sumVersion xs
    sumVersion (OperatorPacket version identifier packets : xs) = version + sumVersion packets + sumVersion xs

run1 :: String -> String
run1 = show . process1 . read . concatMap hexToBit . trim

process2 :: Input -> Int
process2 (Input packets) = head $ execute <$> packets
  where
    execute (LiteralPacket _ value) = value
    execute (OperatorPacket _ 0 packets) = sum $ execute <$> packets
    execute (OperatorPacket _ 1 packets) = product $ execute <$> packets
    execute (OperatorPacket _ 2 packets) = minimum $ execute <$> packets
    execute (OperatorPacket _ 3 packets) = maximum $ execute <$> packets
    execute (OperatorPacket _ 5 (a : b : _)) = if execute a > execute b then 1 else 0
    execute (OperatorPacket _ 6 (a : b : _)) = if execute a < execute b then 1 else 0
    execute (OperatorPacket _ 7 (a : b : _)) = if execute a == execute b then 1 else 0
    execute (OperatorPacket _ o packets) = error $ "Unknown operator : " <> show o

run2 :: String -> String
run2 = show . process2 . read . concatMap hexToBit . trim
