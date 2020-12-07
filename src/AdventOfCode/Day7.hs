module AdventOfCode.Day7
  ( run1,
    process1,
    run2,
    process2,
    Bag,
    Rules,
    ruleParser,
    rulesParser,
  )
where

import Data.List (nub, unfoldr)
import Data.Map (Map, empty, fromList, keys, (!))
import Text.ParserCombinators.ReadP (choice, sepBy, skipSpaces, string)
import Text.ParserCombinators.ReadPrec (ReadPrec, minPrec, readPrec_to_P, readPrec_to_S)
import Text.Read (Lexeme (Ident), lexP, lift, readPrec)

type Bag = String

bagParser :: ReadPrec Bag
bagParser = do
  Ident f <- lexP
  lift skipSpaces
  Ident s <- lexP
  return $ f <> " " <> s

type Rules = Map Bag [Bag]

ruleParser :: ReadPrec (Bag, [Bag])
ruleParser = do
  container <- bagParser
  lift $ string " bags contain "
  contents <- contentsParser
  lift $ string "."
  return (container, contents)
  where
    contentsParser :: ReadPrec [Bag]
    contentsParser =
      fmap concat $
        lift $
          choice
            [ sepBy
                ( readPrec_to_P
                    ( do
                        nb <- (readPrec :: ReadPrec Int)
                        lift skipSpaces
                        bag <- bagParser
                        lift $ choice [string " bag", string " bags"]
                        return $ replicate nb bag
                    )
                    minPrec
                )
                (string ", "),
              [] <$ string "no other bags"
            ]

rulesParser :: ReadPrec Rules
rulesParser = do
  rules <- lift $ sepBy (readPrec_to_P ruleParser minPrec) (string "\n")
  lift $ string "\n"
  return $ fromList rules

readRules :: String -> Rules
readRules = fromList . fmap (fst . head . readPrec_to_S ruleParser minPrec) . lines

run1 :: String -> String
run1 input = show $ length $ filter (process1 rules "shiny gold") $ keys rules
  where
    rules = readRules input

process1 :: Rules -> Bag -> Bag -> Bool
process1 rules content container = elem content $ concat $ unfoldr go [container]
  where
    go [] = Nothing
    go (x : xs) =
      return (bags, nub $ xs <> bags)
      where
        bags = rules ! x

run2 :: String -> String
run2 = id

process2 :: Int -> Int
process2 = id
