module AdventOfCode.Year2024.Day02Spec
  ( spec,
  )
where

import AdventOfCode.Year2024.Day02
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
          [ "7 6 4 2 1",
            "1 2 7 8 9",
            "9 7 6 2 1",
            "1 3 2 4 5",
            "8 6 4 4 1",
            "1 3 6 7 9"
          ]
      )
      `shouldBe` "2"

  it "run2" $ do
    run2
      ( unlines
          [ "7 6 4 2 1",
            "1 2 7 8 9",
            "9 7 6 2 1",
            "1 3 2 4 5",
            "8 6 4 4 1",
            "1 3 6 7 9"
          ]
      )
      `shouldBe` "4"
