module AdventOfCode.Year2024.Day01Spec
  ( spec,
  )
where

import AdventOfCode.Year2024.Day01
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
          [ "3   4",
            "4   3",
            "2   5",
            "1   3",
            "3   9",
            "3   3"
          ]
      )
      `shouldBe` "11"

  it "run2" $ do
    run2
      ( unlines
          [ "3   4",
            "4   3",
            "2   5",
            "1   3",
            "3   9",
            "3   3"
          ]
      )
      `shouldBe` "31"
