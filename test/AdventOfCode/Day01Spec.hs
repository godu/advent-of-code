module AdventOfCode.Day01Spec
  ( spec,
  )
where

import AdventOfCode.Day01
  ( combinaisons,
    process1,
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
  it "combinaisons" $ do
    combinaisons 0 [1, 2, 3, 4] `shouldBe` []
    combinaisons 1 [1, 2, 3, 4] `shouldBe` [[1], [2], [3], [4]]
    combinaisons 2 [1, 2, 3, 4] `shouldBe` [[1, 2], [1, 3], [1, 4], [2, 3], [2, 4], [3, 4]]
    combinaisons 3 [1, 2, 3, 4] `shouldBe` [[1, 2, 3], [1, 2, 4], [1, 3, 4], [2, 3, 4]]
  it "process1" $ do
    process1 [1721, 979, 366, 299, 675, 1456] `shouldBe` 514579
  it "run1" $ do
    run1 "1721\n979\n366\n299\n675\n1456\n" `shouldBe` "514579"
  it "process2" $ do
    process2 [1721, 979, 366, 299, 675, 1456] `shouldBe` 241861950
  it "run2" $ do
    run2 "1721\n979\n366\n299\n675\n1456\n" `shouldBe` "241861950"
