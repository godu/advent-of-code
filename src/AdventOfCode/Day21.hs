{-# LANGUAGE TupleSections #-}

module AdventOfCode.Day21
  ( run1,
    process1,
    run2,
    process2,
  )
where

import Data.Char (isLetter)
import qualified Data.Map as M (Map, elems, empty, filter, fromList, partition, unionWith)
import Data.Set as S (Set, difference, fromList, intersection, toList, union, unions)
import Text.ParserCombinators.ReadP (munch1, sepBy, skipSpaces, string)
import Text.Read (Read (readPrec), lift)

type Ingredient = String

type Allergen = String

data Food = Food
  { ingredients :: [Ingredient],
    allergens :: [Allergen]
  }
  deriving (Show, Eq)

instance Read Food where
  readPrec = do
    ingredients <- lift $ sepBy (munch1 isLetter) (string " ")
    lift skipSpaces
    lift $ string "(contains "
    allergens <- lift $ sepBy (munch1 isLetter) (string ", ")
    lift $ string ")"
    return $ Food ingredients allergens

process1 :: [Food] -> Int
process1 foods = length $ filter (not . (`elem` allergenIngredients)) allIngredients
  where
    indexPossibleIngredientsByAllergens :: Food -> M.Map Allergen (Set Ingredient)
    indexPossibleIngredientsByAllergens (Food ingredients allergens) =
      M.fromList $ (,S.fromList ingredients) <$> allergens
    allergenIngredients :: [Ingredient]
    allergenIngredients =
      concatMap S.toList $
        filter ((== 1) . length) $
          M.elems $
            reduceAllergenIngredients $
              foldl (M.unionWith S.intersection) M.empty $
                indexPossibleIngredientsByAllergens <$> foods
    reduceAllergenIngredients :: M.Map Allergen (Set Ingredient) -> M.Map Allergen (Set Ingredient)
    reduceAllergenIngredients m =
      if m == reducedAllergens
        then reducedAllergens
        else reduceAllergenIngredients reducedAllergens
      where
        (resolvedAllergens, toResolveAllergens) = M.partition ((== 1) . length) m
        resolvedIngredients = S.unions $ M.elems resolvedAllergens
        reducedAllergens = resolvedAllergens <> ((`S.difference` resolvedIngredients) <$> toResolveAllergens)

    allIngredients :: [Ingredient]
    allIngredients = concatMap ingredients foods

run1 :: String -> String
run1 = show . process1 . fmap read . lines

process2 :: Int -> Int
process2 = id

run2 :: String -> String
run2 = const ""
