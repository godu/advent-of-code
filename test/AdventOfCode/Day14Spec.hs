module AdventOfCode.Day14Spec
  ( spec,
  )
where

import AdventOfCode.Day14
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
      "mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X\n\
      \mem[8] = 11\n\
      \mem[7] = 101\n\
      \mem[8] = 0\n"
      `shouldBe` "165"
  it "process2" $ do
    process2 0 `shouldBe` 0
  it "run2" $ do
    run2 "" `shouldBe` ""
