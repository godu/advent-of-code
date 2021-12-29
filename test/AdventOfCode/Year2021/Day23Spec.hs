module AdventOfCode.Year2021.Day23Spec
  ( spec,
  )
where

import AdventOfCode.Year2021.Day23
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
    run1 (unlines ["BA", "CD", "BC", "DA"])
      `shouldBe` "12521"

  it "run2" $ do
    run2 (unlines ["BA", "CD", "BC", "DA"])
      `shouldBe` "44169"
