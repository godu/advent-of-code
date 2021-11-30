module AdventOfCode.Year2020.Day13Spec
  ( spec,
  )
where

import AdventOfCode.Year2020.Day13
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
      "939\n\
      \7,13,x,x,59,x,31,19\n"
      `shouldBe` "295"
  it "run2" $ do
    run2
      "939\n\
      \7,13,x,x,59,x,31,19\n"
      `shouldBe` "1068781"
    run2
      "1068781\n\
      \17,x,13,19\n"
      `shouldBe` "3417"
    run2
      "1068781\n\
      \67,7,59,61\n"
      `shouldBe` "754018"
    run2
      "1068781\n\
      \67,x,7,59,61\n"
      `shouldBe` "779210"
    run2
      "1068781\n\
      \67,7,x,59,61\n"
      `shouldBe` "1261476"
    run2
      "1068781\n\
      \1789,37,47,1889\n"
      `shouldBe` "1202161486"
