module AdventOfCode.Year2021.Day06Spec
  ( spec,
  )
where

import AdventOfCode.Year2021.Day06
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
          [ "3,4,3,1,2"
          ]
      )
      `shouldBe` "5934"

  it "run2" $ do
    run2
      ( unlines
          [ "3,4,3,1,2"
          ]
      )
      `shouldBe` "26984457539"
