module AdventOfCode.Year2021.Day09Spec
  ( spec,
  )
where

import AdventOfCode.Year2021.Day09
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
          [ "2199943210",
            "3987894921",
            "9856789892",
            "8767896789",
            "9899965678"
          ]
      )
      `shouldBe` "15"

  it "run2" $ do
    run2
      ( unlines
          [ "2199943210",
            "3987894921",
            "9856789892",
            "8767896789",
            "9899965678"
          ]
      )
      `shouldBe` "1134"
