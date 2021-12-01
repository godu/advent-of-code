module AdventOfCode.Year2021.Day01Spec
  ( spec,
  )
where

import AdventOfCode.Year2021.Day01
  ( process1,
    process2,
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
  it "process1" $ do
    process1
      [ 199,
        200,
        208,
        210,
        200,
        207,
        240,
        269,
        260,
        263
      ]
      `shouldBe` 7
  it "run1" $ do
    run1
      ( unlines
          [ "199",
            "200",
            "208",
            "210",
            "200",
            "207",
            "240",
            "269",
            "260",
            "263"
          ]
      )
      `shouldBe` "7"
  it "process2" $ do
    process2 0
      `shouldBe` 0
  it "run2" $ do
    run2 "" `shouldBe` ""
