module AdventOfCode.Year2021.Day02Spec
  ( spec,
  )
where

import AdventOfCode.Year2021.Day02
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
  it "run1" $ do
    run1
      ( unlines
          [ "forward 5",
            "down 5",
            "forward 8",
            "up 3",
            "down 8",
            "forward 2"
          ]
      )
      `shouldBe` "150"
  it "run2" $ do
    run2
      ( unlines
          [ "forward 5",
            "down 5",
            "forward 8",
            "up 3",
            "down 8",
            "forward 2"
          ]
      )
      `shouldBe` "900"
