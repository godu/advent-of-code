module AdventOfCode.Day4
  ( run1,
    run2,
    process2,
  )
where

import Data.List.Extra (splitOn)
import Data.Maybe (mapMaybe)
import Text.Read
  ( readMaybe,
  )

data Field
  = BirthYear String
  | IssueYear String
  | ExpirationYear String
  | Height String
  | HairColor String
  | EyeColor String
  | PassportId String
  | CountryId String
  deriving (Show)

toField :: [String] -> Maybe Field
toField ["byr", x] = return $ BirthYear x
toField ["iyr", x] = return $ IssueYear x
toField ["eyr", x] = return $ ExpirationYear x
toField ["hgt", x] = return $ Height x
toField ["hcl", x] = return $ HairColor x
toField ["ecl", x] = return $ EyeColor x
toField ["pid", x] = return $ PassportId x
toField ["cid", x] = return $ CountryId x
toField _ = Nothing

data Passport = Passport
  { birthYear :: String,
    issueYear :: String,
    expirationYear :: String,
    height :: String,
    hairColor :: String,
    eyeColor :: String,
    passportId :: String,
    countryId :: Maybe String
  }
  deriving (Show)

toPassport :: [Field] -> Maybe Passport
toPassport fields =
  case ( getBirthYear fields,
         getIssueYear fields,
         getExpirationYear fields,
         getHeight fields,
         getHairColor fields,
         getEyeColor fields,
         getPassportId fields,
         getCountryId fields
       ) of
    ( Just birthYear,
      Just issueYear,
      Just expirationYear,
      Just height,
      Just hairColor,
      Just eyeColor,
      Just passportId,
      countryId
      ) ->
        return $
          Passport
            birthYear
            issueYear
            expirationYear
            height
            hairColor
            eyeColor
            passportId
            countryId
    _ -> Nothing
  where
    getBirthYear :: [Field] -> Maybe String
    getBirthYear =
      foldl
        ( \acc field -> case (acc, field) of
            (Nothing, BirthYear x) -> Just x
            (x, _) -> x
        )
        Nothing
    getIssueYear :: [Field] -> Maybe String
    getIssueYear =
      foldl
        ( \acc field -> case (acc, field) of
            (Nothing, IssueYear x) -> Just x
            (x, _) -> x
        )
        Nothing
    getExpirationYear :: [Field] -> Maybe String
    getExpirationYear =
      foldl
        ( \acc field -> case (acc, field) of
            (Nothing, ExpirationYear x) -> Just x
            (x, _) -> x
        )
        Nothing
    getHeight :: [Field] -> Maybe String
    getHeight =
      foldl
        ( \acc field -> case (acc, field) of
            (Nothing, Height x) -> Just x
            (x, _) -> x
        )
        Nothing
    getHairColor :: [Field] -> Maybe String
    getHairColor =
      foldl
        ( \acc field -> case (acc, field) of
            (Nothing, HairColor x) -> Just x
            (x, _) -> x
        )
        Nothing
    getEyeColor :: [Field] -> Maybe String
    getEyeColor =
      foldl
        ( \acc field -> case (acc, field) of
            (Nothing, EyeColor x) -> Just x
            (x, _) -> x
        )
        Nothing
    getPassportId :: [Field] -> Maybe String
    getPassportId =
      foldl
        ( \acc field -> case (acc, field) of
            (Nothing, PassportId x) -> Just x
            (x, _) -> x
        )
        Nothing
    getCountryId :: [Field] -> Maybe String
    getCountryId =
      foldl
        ( \acc field -> case (acc, field) of
            (Nothing, CountryId x) -> Just x
            (x, _) -> x
        )
        Nothing

run1 :: String -> String
run1 =
  show
    . length
    . mapMaybe (toPassport . mapMaybe (toField . splitOn ":") . words)
    . splitOn "\n\n"

run2 :: String -> String
run2 = id

process2 :: Int -> Int
process2 = id
