module AdventOfCode.Utils (read', readMaybe', singleton) where

import Text.ParserCombinators.ReadP (skipSpaces)
import Text.ParserCombinators.ReadPrec (minPrec, readPrec_to_S)
import Text.Read (ReadPrec, lift)

readMaybe' :: ReadPrec a -> String -> Maybe a
readMaybe' readPrec s =
  case readEither' readPrec s of
    Right x -> return x
    Left _ -> Nothing

read' :: ReadPrec a -> String -> a
read' readPrec s =
  case readEither' readPrec s of
    Right x -> x
    Left x -> errorWithoutStackTrace x

readEither' :: ReadPrec a -> String -> Either String a
readEither' readPrec s =
  case [x | (x, "") <- readPrec_to_S read' minPrec s] of
    [x] -> Right x
    [] -> Left "Prelude.read: no parse"
    _ -> Left "Prelude.read: ambiguous parse"
  where
    read' =
      do
        x <- readPrec
        lift skipSpaces
        return x

singleton :: a -> [a]
singleton x = [x]
