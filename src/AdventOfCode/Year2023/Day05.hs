module AdventOfCode.Year2023.Day05
  ( run1,
    run2,
  )
where

import Control.Applicative ((<|>))
import Data.Foldable (Foldable (fold), find)
import Data.Ix (Ix (inRange))
import Data.List (sortOn)
import Data.Maybe (fromMaybe)
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

data Almanac' = Almanac'
  { seeds' :: [(Int, Int)],
    seedToSoilMap' :: [(Int, Int, Int)],
    soilToFertilizerMap' :: [(Int, Int, Int)],
    fertilizerToWaterMap' :: [(Int, Int, Int)],
    waterToLightMap' :: [(Int, Int, Int)],
    lightToTemperatureMap' :: [(Int, Int, Int)],
    temperatureToHumidityMap' :: [(Int, Int, Int)],
    humidityToLocationMap' :: [(Int, Int, Int)]
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
      sortBySourceRangeStart :: [(Int, Int, Int)] -> [(Int, Int, Int)]
      sortBySourceRangeStart = sortOn (\(_, sourceRangeStart, _) -> sourceRangeStart)
      readPrecMapping :: ReadPrec (Int, Int, Int)
      readPrecMapping = do
        a <- readPrec
        lift $ char ' '
        b <- readPrec
        lift $ char ' '
        c <- readPrec
        return (a, b, c)
      readPrecRange :: ReadPrec (Int, Int)
      readPrecRange = do
        a <- readPrec
        lift $ char ' '
        b <- readPrec
        return (a, b)

process2 :: Almanac' -> Int
process2 almanac =
  minimum $
    fst
      <$> ( humidityToLocation almanac
              =<< temperatureToHumidity almanac
              =<< lightToTemperature almanac
              =<< waterToLight almanac
              =<< fertilizerToWater almanac
              =<< soilToFertilizer almanac
              =<< seedToSoil almanac
              =<< seeds' almanac
          )
  where
    createTransition :: (Almanac' -> [(Int, Int, Int)]) -> Almanac' -> (Int, Int) -> [(Int, Int)]
    createTransition _ _ (_, 0) = []
    createTransition getMapping almanac (start, length) =
      fromMaybe [(start, length)] $
        buildInMapping (start, length)
          <$> findMapping mappings start
          <|> buildAfterMapping (start, length)
            <$> findNextMapping mappings start
      where
        mappings = getMapping almanac
        buildInMapping :: (Int, Int) -> (Int, Int, Int) -> [(Int, Int)]
        buildInMapping (start, length) (destinationRangeStart, sourceRangeStart, rangeLength) =
          if end < sourceRangeEnd
            then [(nextStart, length)]
            else
              (nextStart, nextLength)
                : createTransition getMapping almanac (start + nextLength, length - nextLength)
          where
            end = start + length
            sourceRangeEnd = sourceRangeStart + rangeLength
            nextStart = destinationRangeStart + (start - sourceRangeStart)
            nextLength = length - (end - sourceRangeEnd)

        buildAfterMapping :: (Int, Int) -> (Int, Int, Int) -> [(Int, Int)]
        buildAfterMapping (start, length) (destinationRangeStart, sourceRangeStart, rangeLength) =
          if end < sourceRangeStart
            then [(start, length)]
            else
              (start, nextLength)
                : createTransition getMapping almanac (sourceRangeStart, length - nextLength)
          where
            end = start + length
            sourceRangeEnd = sourceRangeStart + rangeLength
            nextLength = sourceRangeStart - start

        findMapping :: [(Int, Int, Int)] -> Int -> Maybe (Int, Int, Int)
        findMapping mappings seed = matchWithRange seed `find` mappings
          where
            matchWithRange seed (_, sourceRangeStart, rangeLength) =
              (sourceRangeStart, sourceRangeStart + rangeLength - 1) `inRange` seed
        findNextMapping :: [(Int, Int, Int)] -> Int -> Maybe (Int, Int, Int)
        findNextMapping mappings seed = (\(_, sourceRangeStart, _) -> seed <= sourceRangeStart) `find` mappings

    seedToSoil :: Almanac' -> (Int, Int) -> [(Int, Int)]
    seedToSoil = createTransition seedToSoilMap'
    soilToFertilizer :: Almanac' -> (Int, Int) -> [(Int, Int)]
    soilToFertilizer = createTransition soilToFertilizerMap'
    fertilizerToWater :: Almanac' -> (Int, Int) -> [(Int, Int)]
    fertilizerToWater = createTransition fertilizerToWaterMap'
    waterToLight :: Almanac' -> (Int, Int) -> [(Int, Int)]
    waterToLight = createTransition waterToLightMap'
    lightToTemperature :: Almanac' -> (Int, Int) -> [(Int, Int)]
    lightToTemperature = createTransition lightToTemperatureMap'
    temperatureToHumidity :: Almanac' -> (Int, Int) -> [(Int, Int)]
    temperatureToHumidity = createTransition temperatureToHumidityMap'
    humidityToLocation :: Almanac' -> (Int, Int) -> [(Int, Int)]
    humidityToLocation = createTransition humidityToLocationMap'

run2 :: String -> String
run2 = show . process2 . read
