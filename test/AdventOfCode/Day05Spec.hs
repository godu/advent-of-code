module AdventOfCode.Day05Spec
  ( spec,
  )
where

import AdventOfCode.Day05
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
    process1 "FFFFFFFLLL" `shouldBe` 0
    process1 "BBBBBBBRRR" `shouldBe` 127 * 8 + 7

    process1 "BFFFBBFRRR" `shouldBe` 567
    process1 "FFFBBBFRRR" `shouldBe` 119
    process1 "BBFFBBFRLL" `shouldBe` 820
  it "run1" $ do
    run1 "BFFFBBFRRR\nFFFBBBFRRR\nBBFFBBFRLL" `shouldBe` "820"
