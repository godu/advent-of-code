module AdventOfCode.Year2021.DayXXSpec
  ( spec,
  )
where

import AdventOfCode.Year2021.DayXX
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
    run1 (unlines []) `shouldBe` ""

  it "run2" $ do
    run2 (unlines []) `shouldBe` ""
