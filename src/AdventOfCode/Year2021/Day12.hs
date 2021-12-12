{-# LANGUAGE LambdaCase #-}

module AdventOfCode.Year2021.Day12
  ( run1,
    run2,
  )
where

import Data.Bifunctor (Bifunctor (bimap))
import Data.Char (isLower, isUpper)
import Data.List.Extra (nub, stripInfix, (\\))
import Data.Map as M (Map, fromList, unionsWith, (!), (!?))
import Data.Maybe (fromJust)
import Data.Set as S (Set, fromList, toList)
import Debug.Trace (traceShow, traceShowId)
import GHC.Exts (groupWith)
import Text.ParserCombinators.ReadP (munch1, string, (<++))
import Text.Read (lift, minPrec, pfail, readPrec)

data Cave = Start | Big String | Small String | End deriving (Eq, Show, Ord)

instance Read Cave where
  readPrec =
    lift $
      (Start <$ string "start")
        <++ (End <$ string "end")
        <++ (Small <$> munch1 isLower)
        <++ (Big <$> munch1 isUpper)

parse :: String -> Map Cave (Set Cave)
parse =
  unionsWith (<>)
    . map
      ( ( \(a, b) ->
            M.fromList
              [ (a, S.fromList [b]),
                (b, S.fromList [a])
              ]
        )
          . bimap read read
          . fromJust
          . stripInfix "-"
      )
    . lines

process1 :: Map Cave (Set Cave) -> Int
process1 = length . visite
  where
    visite :: Map Cave (Set Cave) -> [[Cave]]
    visite map = go map [Start]
      where
        go :: Map Cave (Set Cave) -> [Cave] -> [[Cave]]
        go map [] = []
        go map path@(End : _) = [path]
        go map path@(prev : _) =
          concatMap
            ( go map . (: path)
            )
            $ (`restrict` path) $
              S.toList $
                map ! prev
        possibilities `restrict` path =
          filter
            ( \case
                Start -> Start `notElem` path
                Small n -> Small n `notElem` path
                Big _ -> True
                End -> End `notElem` path
            )
            possibilities

run1 :: String -> String
run1 = show . process1 . parse

process2 :: Map Cave (Set Cave) -> Int
process2 = length . visite
  where
    visite :: Map Cave (Set Cave) -> [[Cave]]
    visite map = go map [Start]
      where
        go :: Map Cave (Set Cave) -> [Cave] -> [[Cave]]
        go map [] = []
        go map path@(End : _) = [path]
        go map path@(prev : _) =
          concatMap
            ( go map . (: path)
            )
            $ (`restrict` path) $
              S.toList $
                map ! prev
        possibilities `restrict` path =
          filter
            ( \case
                Start -> Start `notElem` path
                Small n
                  | hasAlreadyVisitingACaveTwoTimes -> Small n `notElem` path
                  | otherwise -> (<= 2) $ length $ filter (== Small n) path
                Big _ -> True
                End -> End `notElem` path
            )
            possibilities
          where
            smallCaves =
              filter
                ( \case
                    Small n -> True
                    _ -> False
                )
                path
            hasAlreadyVisitingACaveTwoTimes = smallCaves /= nub smallCaves

run2 :: String -> String
run2 = show . process2 . parse
