module AdventOfCode.Day19
  ( run1,
    process1,
    run2,
    process2,
  )
where

import AdventOfCode.Utils (read', readMaybe')
import Control.Monad (foldM)
import Data.Char (isDigit, isLetter)
import Data.IntMap (IntMap, fromList, (!?))
import Data.Maybe (mapMaybe)
import Text.ParserCombinators.ReadP (char, eof, munch1, sepBy1, skipSpaces, string)
import Text.Read (ReadPrec, get, lift, minPrec, readPrec, readPrec_to_P, (+++))

newtype Rule = Rule (IntMap Rule -> ReadPrec String)

readPrecInt :: ReadPrec Int
readPrecInt = readPrec

readPrecCharRule :: ReadPrec Rule
readPrecCharRule = do
  '"' <- get
  a <- get
  '"' <- get
  return $ Rule $ const $ lift $ string [a]

readPrecAndRule :: ReadPrec Rule
readPrecAndRule = do
  a <- lift $ sepBy1 (readPrec_to_P readPrecInt minPrec) (string " ")
  return $
    Rule $
      ( \m ->
          foldM
            ( \a r -> do
                r' <- r
                return $ a <> r'
            )
            ""
            $ (\(Rule rule) -> rule m) <$> mapMaybe (m !?) a
      )

readPrecOrRule :: ReadPrec Rule
readPrecOrRule = do
  a <- lift $ sepBy1 (readPrec_to_P readPrecAndRule minPrec) (string " | ")
  return $ Rule $ (\m -> foldl1 (+++) $ (\(Rule rule) -> rule m) <$> a)

readPrecRules :: ReadPrec (IntMap Rule)
readPrecRules =
  fromList
    <$> lift
      ( sepBy1
          (readPrec_to_P readPrecRules' minPrec)
          (string "\n")
      )
  where
    readPrecRules' :: ReadPrec (Int, Rule)
    readPrecRules' = do
      i <- readPrec
      ':' <- get
      lift skipSpaces
      rule <- readPrecCharRule +++ readPrecOrRule
      return (i, rule)

readPrecInput :: ReadPrec (IntMap Rule, [String])
readPrecInput = do
  rules <- readPrecRules
  lift $ string "\n\n"
  inputs <-
    lift $
      sepBy1
        (munch1 isLetter)
        (string "\n")
  lift skipSpaces
  lift $ eof
  return (rules, inputs)

process1 :: IntMap Rule -> String -> Maybe String
process1 m a = do
  Rule rule <- m !? 0
  readMaybe' (rule m) a

run1 :: String -> String
run1 a =
  show $
    length $
      ( \(rules, inputs) ->
          mapMaybe
            (process1 rules)
            inputs
      )
        $ read' readPrecInput a

process2 :: Int -> Int
process2 = id

run2 :: String -> String
run2 = const ""
