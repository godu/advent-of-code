module AdventOfCode.Day10Spec
  ( spec,
  )
where

import AdventOfCode.Day10
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
  it "process1" $ do
    process1 [16, 10, 15, 5, 1, 11, 7, 19, 6, 12, 4] `shouldBe` 35
    process1 [28, 33, 18, 42, 31, 14, 46, 20, 48, 47, 24, 23, 49, 45, 19, 38, 39, 11, 1, 32, 25, 35, 8, 17, 7, 9, 4, 2, 34, 10, 3] `shouldBe` 220
  it "run1" $ do
    run1
      "16\n\
      \10\n\
      \15\n\
      \5\n\
      \1\n\
      \11\n\
      \7\n\
      \19\n\
      \6\n\
      \12\n\
      \4\n"
      `shouldBe` "35"
  it "process2" $ do
    process2 0 `shouldBe` 0
  it "run2" $ do
    run2 "0" `shouldBe` "0"
