module AdventOfCode.Day18Spec
  ( spec,
  )
where

import AdventOfCode.Day18
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
  it "run1" $ do
    run1 "1 + 2 * 3 + 4 * 5 + 6" `shouldBe` "71"
    run1 "1 + (2 * 3) + (4 * (5 + 6))" `shouldBe` "51"
    run1 "1 + (2 * 3) + (4 * (5 + 6))" `shouldBe` "51"
    run1 "2 * 3 + (4 * 5)" `shouldBe` "26"
    run1 "5 + (8 * 3 + 9 + 3 * 4 * 3)" `shouldBe` "437"
    run1 "5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))" `shouldBe` "12240"
    run1 "((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2" `shouldBe` "13632"
  it "process2" $ do
    process2 0 `shouldBe` 0
  it "run2" $ do
    run2 "" `shouldBe` ""
