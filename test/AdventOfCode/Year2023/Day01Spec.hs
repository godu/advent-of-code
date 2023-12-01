module AdventOfCode.Year2023.Day01Spec
  ( spec,
  )
where

import AdventOfCode.Year2023.Day01
  ( run1,
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
      ( unlines
          [ "1abc2",
            "pqr3stu8vwx",
            "a1b2c3d4e5f",
            "treb7uchet"
          ]
      )
      `shouldBe` "142"

  it "run2" $ do
    run2
      ( unlines
          [ "two1nine",
            "eightwothree",
            "abcone2threexyz",
            "xtwone3four",
            "4nineeightseven2",
            "zoneight234",
            "7pqrstsixteen"
          ]
      )
      `shouldBe` "281"
