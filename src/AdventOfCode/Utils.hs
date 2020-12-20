module AdventOfCode.Utils (read', readMaybe') where

import Debug.Trace
import Text.ParserCombinators.ReadP (skipSpaces)
import Text.ParserCombinators.ReadPrec (minPrec, readPrec_to_S)
import Text.Read (ReadPrec, lift)

readMaybe' :: ReadPrec a -> String -> Maybe a
readMaybe' readPrec s =
  case [x | (x, "") <- readPrec_to_S read' minPrec s] of
    [x] -> Just x
    _ -> Nothing
  where
    read' =
      do
        x <- readPrec
        lift skipSpaces
        return x

read' :: ReadPrec a -> String -> a
read' readPrec s =
  case [x | (x, "") <- readPrec_to_S read' minPrec s] of
    [x] -> x
    [] -> errorWithoutStackTrace "Prelude.read: no parse"
    _ -> errorWithoutStackTrace "Prelude.read: ambiguous parse"
  where
    read' =
      do
        x <- readPrec
        lift skipSpaces
        return x
