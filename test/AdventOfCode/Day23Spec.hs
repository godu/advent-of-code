module AdventOfCode.Day23Spec
  ( spec,
  )
where

import AdventOfCode.Day23
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
    process1 0 [3, 8, 9, 1, 2, 5, 4, 6, 7] `shouldBe` [2, 5, 4, 6, 7, 3, 8, 9]
    process1 10 [3, 8, 9, 1, 2, 5, 4, 6, 7] `shouldBe` [9, 2, 6, 5, 8, 3, 7, 4]
  it "run1" $ do
    run1 "389125467" `shouldBe` "67384529"
  it "process2" $ do
    process2 0 `shouldBe` 0
  it "run2" $ do
    run2 "" `shouldBe` ""
