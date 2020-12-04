module AdventOfCode.Day4
  ( Passport (..),
    run1,
    run2,
    process1,
    process2,
  )
where

import Control.Applicative (Applicative (liftA2))
import Data.Bifunctor (Bifunctor (first))
import Data.Char (isDigit, isHexDigit)
import Data.Ix (Ix (inRange))
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
  deriving (Show, Eq)

toPassport :: [Field] -> Maybe Passport
toPassport fields =
  case ( foldMap getBirthYear fields,
         foldMap getIssueYear fields,
         foldMap getExpirationYear fields,
         foldMap getHeight fields,
         foldMap getHairColor fields,
         foldMap getEyeColor fields,
         foldMap getPassportId fields,
         foldMap getCountryId fields
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
    getBirthYear :: Field -> Maybe String
    getBirthYear (BirthYear x) = Just x
    getBirthYear _ = Nothing

    getIssueYear :: Field -> Maybe String
    getIssueYear (IssueYear x) = Just x
    getIssueYear _ = Nothing

    getExpirationYear :: Field -> Maybe String
    getExpirationYear (ExpirationYear x) = Just x
    getExpirationYear _ = Nothing

    getHeight :: Field -> Maybe String
    getHeight (Height x) = Just x
    getHeight _ = Nothing

    getHairColor :: Field -> Maybe String
    getHairColor (HairColor x) = Just x
    getHairColor _ = Nothing

    getEyeColor :: Field -> Maybe String
    getEyeColor (EyeColor x) = Just x
    getEyeColor _ = Nothing

    getPassportId :: Field -> Maybe String
    getPassportId (PassportId x) = Just x
    getPassportId _ = Nothing

    getCountryId :: Field -> Maybe String
    getCountryId (CountryId x) = Just x
    getCountryId _ = Nothing

run1 :: String -> String
run1 =
  show
    . length
    . mapMaybe process1
    . splitOn "\n\n"

process1 :: String -> Maybe Passport
process1 = toPassport . mapMaybe (toField . splitOn ":") . words

run2 :: String -> String
run2 =
  show
    . length
    . mapMaybe process2
    . splitOn "\n\n"

(<?>) :: (t -> Bool) -> (t -> Bool) -> t -> Bool
a <?> b = liftA2 (&&) a b

process2 :: String -> Maybe Passport
process2 =
  filter
    ( isPassportIdValid
        <?> isEyeColorValid
        <?> isHairColorValid
        <?> isHeightValid
        <?> isExpirationYearValid
        <?> isIssueYearValid
        <?> isBirthYearValid
    )
    . process1
  where
    filter predicate =
      maybe
        Nothing
        (\p -> if predicate p then Just p else Nothing)

    isBirthYearValid (Passport [a, b, c, d] _ _ _ _ _ _ _) =
      maybe
        False
        (inRange (1920, 2002))
        $ readMaybe [a, b, c, d]
    isBirthYearValid _ = False

    isIssueYearValid (Passport _ [a, b, c, d] _ _ _ _ _ _) =
      maybe
        False
        (inRange (2010, 2020))
        $ readMaybe [a, b, c, d]
    isIssueYearValid _ = False

    isExpirationYearValid (Passport _ _ [a, b, c, d] _ _ _ _ _) =
      maybe
        False
        (inRange (2020, 2030))
        $ readMaybe [a, b, c, d]
    isExpirationYearValid _ = False

    isHeightValid (Passport _ _ _ h _ _ _ _) = case first readMaybe $ span isDigit h of
      (Just value, "cm") -> inRange (150, 193) value
      (Just value, "in") -> inRange (59, 76) value
      _ -> False

    isHairColorValid (Passport _ _ _ _ ['#', b, c, d, e, f, g] _ _ _) = all isHexDigit [b, c, d, e, f, g]
    isHairColorValid _ = False

    isEyeColorValid (Passport _ _ _ _ _ "amb" _ _) = True
    isEyeColorValid (Passport _ _ _ _ _ "blu" _ _) = True
    isEyeColorValid (Passport _ _ _ _ _ "brn" _ _) = True
    isEyeColorValid (Passport _ _ _ _ _ "gry" _ _) = True
    isEyeColorValid (Passport _ _ _ _ _ "grn" _ _) = True
    isEyeColorValid (Passport _ _ _ _ _ "hzl" _ _) = True
    isEyeColorValid (Passport _ _ _ _ _ "oth" _ _) = True
    isEyeColorValid _ = False

    isPassportIdValid (Passport _ _ _ _ _ _ [a, b, c, d, e, f, g, h, i] _) = all isDigit [a, b, c, d, e, f, g, h, i]
    isPassportIdValid _ = False
