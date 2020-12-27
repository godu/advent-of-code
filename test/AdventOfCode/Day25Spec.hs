module AdventOfCode.Day25Spec
  ( spec,
  )
where

import AdventOfCode.Day25
  ( process1,
    run1,
  )
import Test.Hspec
  ( Spec,
    it,
    shouldBe,
  )

spec :: Spec
spec = do
  it "process1" $ do
    process1 (17807724, 5764801) `shouldBe` 14897079
  it "run1" $ do
    run1
      "17807724\n\
      \5764801\n"
      `shouldBe` "14897079"
