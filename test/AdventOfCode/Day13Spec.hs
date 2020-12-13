module AdventOfCode.Day13Spec
  ( spec,
  )
where

import AdventOfCode.Day13
  ( process2,
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
      "939\n\
      \7,13,x,x,59,x,31,19\n"
      `shouldBe` "295"
  it "process2" $ do
    process2 0 `shouldBe` 0
  it "run2" $ do
    run2 "" `shouldBe` ""
