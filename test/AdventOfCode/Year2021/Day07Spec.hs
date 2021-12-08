module AdventOfCode.Year2021.Day07Spec
  ( spec,
  )
where

import AdventOfCode.Year2021.Day07
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
          [ "16,1,2,0,4,2,7,1,2,14"
          ]
      )
      `shouldBe` "37"

  it "run2" $ do
    run2
      ( unlines
          [ "16,1,2,0,4,2,7,1,2,14"
          ]
      )
      `shouldBe` "168"
