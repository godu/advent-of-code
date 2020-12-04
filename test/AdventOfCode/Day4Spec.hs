module AdventOfCode.Day4Spec
  ( spec,
  )
where

import AdventOfCode.Day4
  ( process2,
    run1,
    run2,
  )
import Test.Hspec
  ( Spec,
    it,
    shouldBe,
  )

spec :: Spec
spec = do
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
    process2 0 `shouldBe` 0
  it "run2" $ do
    run2 "0" `shouldBe` "0"
