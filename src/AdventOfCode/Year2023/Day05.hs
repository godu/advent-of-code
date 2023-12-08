module AdventOfCode.Year2023.Day05
  ( run1,
    run2,
  )
where

import Control.Applicative ((<|>))
import Data.Foldable (Foldable (fold), find)
import Data.Ix (inRange)
import Data.List (sortOn, (\\))
import Data.Maybe (fromMaybe)
import Debug.Trace (traceShow)
import Text.ParserCombinators.ReadP (char, sepBy1, string)
import Text.Read (Read (readPrec), ReadPrec, lift, readPrec_to_P, readPrec_to_S)

data Almanac = Almanac
  { seeds :: [Int],
    seedToSoilMap :: [(Int, Int, Int)],
    soilToFertilizerMap :: [(Int, Int, Int)],
    fertilizerToWaterMap :: [(Int, Int, Int)],
    waterToLightMap :: [(Int, Int, Int)],
    lightToTemperatureMap :: [(Int, Int, Int)],
    temperatureToHumidityMap :: [(Int, Int, Int)],
    humidityToLocationMap :: [(Int, Int, Int)]
  }
  deriving (Show)

instance Read Almanac where
  readPrec = do
    lift $ string "seeds: "
    seeds <- lift $ readPrec_to_P readPrec 0 `sepBy1` char ' '
    lift $ string "\n\n"
    lift $ string "seed-to-soil map:"
    lift $ string "\n"
    seedToSoilMap <- lift $ readPrec_to_P readPrecMapping 0 `sepBy1` char '\n'
    lift $ string "\n\n"
    lift $ string "soil-to-fertilizer map:"
    lift $ string "\n"
    soilToFertilizerMap <- lift $ readPrec_to_P readPrecMapping 0 `sepBy1` char '\n'
    lift $ string "\n\n"
    lift $ string "fertilizer-to-water map:"
    lift $ string "\n"
    fertilizerToWaterMap <- lift $ readPrec_to_P readPrecMapping 0 `sepBy1` char '\n'
    lift $ string "\n\n"
    lift $ string "water-to-light map:"
    lift $ string "\n"
    waterToLightMap <- lift $ readPrec_to_P readPrecMapping 0 `sepBy1` char '\n'
    lift $ string "\n\n"
    lift $ string "light-to-temperature map:"
    lift $ string "\n"
    lightToTemperatureMap <- lift $ readPrec_to_P readPrecMapping 0 `sepBy1` char '\n'
    lift $ string "\n\n"
    lift $ string "temperature-to-humidity map:"
    lift $ string "\n"
    temperatureToHumidityMap <- lift $ readPrec_to_P readPrecMapping 0 `sepBy1` char '\n'
    lift $ string "\n\n"
    lift $ string "humidity-to-location map:"
    lift $ string "\n"
    humidityToLocationMap <- lift $ readPrec_to_P readPrecMapping 0 `sepBy1` char '\n'
    return $ Almanac seeds seedToSoilMap soilToFertilizerMap fertilizerToWaterMap waterToLightMap lightToTemperatureMap temperatureToHumidityMap humidityToLocationMap
    where
      readPrecMapping :: ReadPrec (Int, Int, Int)
      readPrecMapping = do
        a <- readPrec
        lift $ char ' '
        b <- readPrec
        lift $ char ' '
        c <- readPrec
        return (a, b, c)

process1 :: Almanac -> Int
process1 alamanac =
  minimum $
    humidityToLocation alamanac
      . temperatureToHumidity alamanac
      . lightToTemperature alamanac
      . waterToLight alamanac
      . fertilizerToWater alamanac
      . soilToFertilizer alamanac
      . seedToSoil alamanac
      <$> seeds alamanac
  where
    findMapping :: [(Int, Int, Int)] -> Int -> Maybe (Int, Int, Int)
    findMapping mappings seed = matchWithRange seed `find` mappings
      where
        matchWithRange seed (_, sourceRangeStart, rangeLength) =
          (sourceRangeStart, sourceRangeStart + rangeLength - 1) `inRange` seed

    applyMapping :: Int -> (Int, Int, Int) -> Int
    applyMapping seed (destinationRangeStart, sourceRangeStart, _) =
      destinationRangeStart + diff
      where
        diff = seed - sourceRangeStart

    createTransition :: (Almanac -> [(Int, Int, Int)]) -> Almanac -> Int -> Int
    createTransition getMappings almanac seed =
      maybe seed (applyMapping seed) (getMappings almanac `findMapping` seed)

    seedToSoil :: Almanac -> Int -> Int
    seedToSoil = createTransition seedToSoilMap
    soilToFertilizer :: Almanac -> Int -> Int
    soilToFertilizer = createTransition soilToFertilizerMap
    fertilizerToWater :: Almanac -> Int -> Int
    fertilizerToWater = createTransition fertilizerToWaterMap
    waterToLight :: Almanac -> Int -> Int
    waterToLight = createTransition waterToLightMap
    lightToTemperature :: Almanac -> Int -> Int
    lightToTemperature = createTransition lightToTemperatureMap
    temperatureToHumidity :: Almanac -> Int -> Int
    temperatureToHumidity = createTransition temperatureToHumidityMap
    humidityToLocation :: Almanac -> Int -> Int
    humidityToLocation = createTransition humidityToLocationMap

run1 :: String -> String
run1 = show . process1 . read

type Range = (Int, Int)

data Mapping = Mapping
  { sourceRange :: Range,
    offset :: Int
  }
  deriving (Show)

data Almanac' = Almanac'
  { seeds' :: [Range],
    seedToSoilMap' :: [Mapping],
    soilToFertilizerMap' :: [Mapping],
    fertilizerToWaterMap' :: [Mapping],
    waterToLightMap' :: [Mapping],
    lightToTemperatureMap' :: [Mapping],
    temperatureToHumidityMap' :: [Mapping],
    humidityToLocationMap' :: [Mapping]
  }
  deriving (Show)

instance Read Almanac' where
  readPrec = do
    lift $ string "seeds: "
    seeds <- lift $ readPrec_to_P readPrecRange 0 `sepBy1` char ' '
    lift $ string "\n\n"
    lift $ string "seed-to-soil map:"
    lift $ string "\n"
    seedToSoilMap <- lift $ readPrec_to_P readPrecMapping 0 `sepBy1` char '\n'
    lift $ string "\n\n"
    lift $ string "soil-to-fertilizer map:"
    lift $ string "\n"
    soilToFertilizerMap <- lift $ readPrec_to_P readPrecMapping 0 `sepBy1` char '\n'
    lift $ string "\n\n"
    lift $ string "fertilizer-to-water map:"
    lift $ string "\n"
    fertilizerToWaterMap <- lift $ readPrec_to_P readPrecMapping 0 `sepBy1` char '\n'
    lift $ string "\n\n"
    lift $ string "water-to-light map:"
    lift $ string "\n"
    waterToLightMap <- lift $ readPrec_to_P readPrecMapping 0 `sepBy1` char '\n'
    lift $ string "\n\n"
    lift $ string "light-to-temperature map:"
    lift $ string "\n"
    lightToTemperatureMap <- lift $ readPrec_to_P readPrecMapping 0 `sepBy1` char '\n'
    lift $ string "\n\n"
    lift $ string "temperature-to-humidity map:"
    lift $ string "\n"
    temperatureToHumidityMap <- lift $ readPrec_to_P readPrecMapping 0 `sepBy1` char '\n'
    lift $ string "\n\n"
    lift $ string "humidity-to-location map:"
    lift $ string "\n"
    humidityToLocationMap <- lift $ readPrec_to_P readPrecMapping 0 `sepBy1` char '\n'
    return $ Almanac' seeds seedToSoilMap soilToFertilizerMap fertilizerToWaterMap waterToLightMap lightToTemperatureMap temperatureToHumidityMap humidityToLocationMap
    where
      sortBySourceRangeStart :: [Mapping] -> [Mapping]
      sortBySourceRangeStart = sortOn (fst . sourceRange)
      readPrecMapping :: ReadPrec Mapping
      readPrecMapping = do
        a <- readPrec
        lift $ char ' '
        b <- readPrec
        lift $ char ' '
        c <- readPrec
        return $ Mapping (b, b + c - 1) (a - b)
      readPrecRange :: ReadPrec Range
      readPrecRange = do
        a <- readPrec
        lift $ char ' '
        b <- readPrec
        return (a, a + b - 1)

process2 :: Almanac' -> Int
process2 almanac =
  minimum $
    fst
      <$> (
            -- humidityToLocation almanac
            --       =<< temperatureToHumidity almanac
            --       =<< lightToTemperature almanac
            --       =<< waterToLight almanac
            --       =<< fertilizerToWater almanac
            --       =<< soilToFertilizer almanac
            --       =<<
            seedToSoil almanac
              =<< seeds' almanac
          )
  where
    matchedMapping range = find (inRange range . sourceRange) (seedToSoilMap' almanac)

    seedToSoil :: Almanac' -> Range -> [Range]
    seedToSoil almanac (start, end) = traceShow ("seedToSoil", start, end) []

    rangesOverlap :: Range -> Range -> Bool
    rangesOverlap (start1, end1) (start2, end2) =
      inRange (start1, end1) start2
        || inRange (start1, end1) end2
        || inRange (start2, end2) start1
        || inRange (start2, end2) end1

    isEmpty = uncurry (==)

    intersection :: Range -> Range -> [Range]
    intersection (start1, end1) (start2, end2) = filter (not . isEmpty) [(max start1 start2, min end1 end2)]

    difference :: Range -> Range -> [Range]
    difference (start1, end1) (start2, end2) =
      intersection (start1, end1) (start2, end2)
        \\ ( split end2
               =<< split start2
               =<< split end1
               =<< split start1
               =<< (min start1 start2, max end1 end2)
           )

    split :: Int -> Range -> [Range]
    split splitPoint (start, end) =
      if inRange (start, end) splitPoint
        then [(start, splitPoint - 1), (splitPoint, end)]
        else [(start, end)]

run2 :: String -> String
run2 = show . process2 . read
