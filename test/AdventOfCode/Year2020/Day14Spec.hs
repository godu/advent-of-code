module AdventOfCode.Year2020.Day14Spec
  ( spec,
  )
where

import AdventOfCode.Year2020.Day14
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
      "mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X\n\
      \mem[8] = 11\n\
      \mem[7] = 101\n\
      \mem[8] = 0\n"
      `shouldBe` "165"
  it "run2" $ do
    run2
      "mask = 000000000000000000000000000000X1001X\n\
      \mem[42] = 100\n\
      \mask = 00000000000000000000000000000000X0XX\n\
      \mem[26] = 1\n"
      `shouldBe` "208"
