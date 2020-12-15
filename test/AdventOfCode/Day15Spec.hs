module AdventOfCode.Day15Spec
  ( spec,
  )
where

import AdventOfCode.Day15
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
    run1 "0,3,6" `shouldBe` "436"
    run1 "1,3,2" `shouldBe` "1"
    run1 "2,1,3" `shouldBe` "10"
    run1 "1,2,3" `shouldBe` "27"
    run1 "2,3,1" `shouldBe` "78"
    run1 "3,2,1" `shouldBe` "438"
    run1 "3,1,2" `shouldBe` "1836"

  it "run2" $ do
    run2 "0,3,6" `shouldBe` "175594"
    run2 "1,3,2" `shouldBe` "2578"
    run2 "2,1,3" `shouldBe` "3544142"
    run2 "1,2,3" `shouldBe` "261214"
    run2 "2,3,1" `shouldBe` "6895259"
    run2 "3,2,1" `shouldBe` "18"
    run2 "3,1,2" `shouldBe` "362"
