module AdventOfCode.Year2020.Day04Spec
  ( spec,
  )
where

import AdventOfCode.Year2020.Day04
  ( Passport (..),
    process1,
    process2,
    run1,
    run2,
  )
import Test.Hspec
  ( Spec,
    it,
    shouldBe,
  )

passport :: Passport
passport =
  Passport
    { birthYear = "1937",
      issueYear = "2017",
      expirationYear = "2020",
      height = "183cm",
      hairColor = "#fffffd",
      eyeColor = "gry",
      passportId = "860033327",
      countryId = Just "147"
    }

spec :: Spec
spec = do
  it "process1" $ do
    process1 "ecl:gry pid:860033327 eyr:2020 hcl:#fffffd byr:1937 iyr:2017 cid:147 hgt:183cm"
      `shouldBe` Just passport
  it "run1" $ do
    run1
      "ecl:gry pid:860033327 eyr:2020 hcl:#fffffd\n\
      \byr:1937 iyr:2017 cid:147 hgt:183cm\n\
      \\n\
      \iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884\n\
      \hcl:#cfa07d byr:1929\n\
      \\n\
      \hcl:#ae17e1 iyr:2013\n\
      \eyr:2024\n\
      \ecl:brn pid:760753108 byr:1931\n\
      \hgt:179cm\n\
      \\n\
      \hcl:#cfa07d eyr:2025 pid:166559648\n\
      \iyr:2011 ecl:brn hgt:59in"
      `shouldBe` "2"
  it "process2" $ do
    process2 "ecl:gry pid:860033327 eyr:2020 hcl:#fffffd byr:2002 iyr:2017 cid:147 hgt:183cm"
      `shouldBe` Just (passport {birthYear = "2002"})
    process2 "ecl:gry pid:860033327 eyr:2020 hcl:#fffffd byr:2003 iyr:2017 cid:147 hgt:183cm"
      `shouldBe` Nothing

    process2 "ecl:gry pid:860033327 eyr:2020 hcl:#fffffd byr:1937 iyr:2017 cid:147 hgt:60in"
      `shouldBe` Just (passport {height = "60in"})
    process2 "ecl:gry pid:860033327 eyr:2020 hcl:#fffffd byr:1937 iyr:2017 cid:147 hgt:190cm"
      `shouldBe` Just (passport {height = "190cm"})
    process2 "ecl:gry pid:860033327 eyr:2020 hcl:#fffffd byr:1937 iyr:2017 cid:147 hgt:190in"
      `shouldBe` Nothing
    process2 "ecl:gry pid:860033327 eyr:2020 hcl:#fffffd byr:1937 iyr:2017 cid:147 hgt:190"
      `shouldBe` Nothing

    process2 "ecl:gry pid:860033327 eyr:2020 hcl:#123abc byr:1937 iyr:2017 cid:147 hgt:183cm"
      `shouldBe` Just (passport {hairColor = "#123abc"})
    process2 "ecl:gry pid:860033327 eyr:2020 hcl:#123abz byr:1937 iyr:2017 cid:147 hgt:183cm"
      `shouldBe` Nothing
    process2 "ecl:gry pid:860033327 eyr:2020 hcl:123abc byr:1937 iyr:2017 cid:147 hgt:183cm"
      `shouldBe` Nothing

    process2 "ecl:brn pid:860033327 eyr:2020 hcl:#fffffd byr:1937 iyr:2017 cid:147 hgt:183cm"
      `shouldBe` Just (passport {eyeColor = "brn"})
    process2 "ecl:wat pid:860033327 eyr:2020 hcl:#fffffd byr:1937 iyr:2017 cid:147 hgt:183cm"
      `shouldBe` Nothing

    process2 "ecl:gry pid:000000001 eyr:2020 hcl:#fffffd byr:1937 iyr:2017 cid:147 hgt:183cm"
      `shouldBe` Just (passport {passportId = "000000001"})
    process2 "ecl:gry pid:0123456789 eyr:2020 hcl:#fffffd byr:1937 iyr:2017 cid:147 hgt:183cm"
      `shouldBe` Nothing

    process2 "eyr:1972 cid:100 hcl:#18171d ecl:amb hgt:170 pid:186cm iyr:2018 byr:1926"
      `shouldBe` Nothing
    process2 "iyr:2019 hcl:#602927 eyr:1967 hgt:170cm ecl:grn pid:012533040 byr:1946"
      `shouldBe` Nothing
    process2 "hcl:dab227 iyr:2012ecl:brn hgt:182cm pid:021572410 eyr:2020 byr:1992 cid:277"
      `shouldBe` Nothing
    process2 "hgt:59cm ecl:zzz eyr:2038 hcl:74454a iyr:2023 pid:3556412378 byr:2007"
      `shouldBe` Nothing

    process2 "pid:087499704 hgt:74in ecl:grn iyr:2012 eyr:2030 byr:1980 hcl:#623a2f"
      `shouldBe` Just
        ( Passport
            { birthYear = "1980",
              issueYear = "2012",
              expirationYear = "2030",
              height = "74in",
              hairColor = "#623a2f",
              eyeColor = "grn",
              passportId = "087499704",
              countryId = Nothing
            }
        )
    process2 "eyr:2029 ecl:blu cid:129 byr:1989 iyr:2014 pid:896056539 hcl:#a97842 hgt:165cm"
      `shouldBe` Just
        ( Passport
            { birthYear = "1989",
              issueYear = "2014",
              expirationYear = "2029",
              height = "165cm",
              hairColor = "#a97842",
              eyeColor = "blu",
              passportId = "896056539",
              countryId = Just "129"
            }
        )
    process2 "hcl:#888785 hgt:164cm byr:2001 iyr:2015 cid:88 pid:545766238 ecl:hzl eyr:2022"
      `shouldBe` Just
        ( Passport
            { birthYear = "2001",
              issueYear = "2015",
              expirationYear = "2022",
              height = "164cm",
              hairColor = "#888785",
              eyeColor = "hzl",
              passportId = "545766238",
              countryId = Just "88"
            }
        )
    process2 "iyr:2010 hgt:158cm hcl:#b6652a ecl:blu byr:1944 eyr:2021 pid:093154719"
      `shouldBe` Just
        ( Passport
            { birthYear = "1944",
              issueYear = "2010",
              expirationYear = "2021",
              height = "158cm",
              hairColor = "#b6652a",
              eyeColor = "blu",
              passportId = "093154719",
              countryId = Nothing
            }
        )

  it "run2" $ do
    run2 "0" `shouldBe` "0"
