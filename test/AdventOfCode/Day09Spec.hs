module AdventOfCode.Day09Spec
  ( spec,
  )
where

import AdventOfCode.Day09
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
    process1
      5
      [35, 20, 15, 25, 47, 40, 62, 55, 65, 95, 102, 117, 150, 182, 127, 219, 299, 277, 309, 576]
      `shouldBe` return 127
  it "run1" $ do
    run1
      "35\n\
      \20\n\
      \15\n\
      \25\n\
      \47\n\
      \40\n\
      \62\n\
      \55\n\
      \65\n\
      \95\n\
      \102\n\
      \117\n\
      \150\n\
      \182\n\
      \127\n\
      \219\n\
      \299\n\
      \277\n\
      \309\n\
      \576\n"
      `shouldBe` "0"
  it "process2" $ do
    process2
      5
      [35, 20, 15, 25, 47, 40, 62, 55, 65, 95, 102, 117, 150, 182, 127, 219, 299, 277, 309, 576]
      `shouldBe` return 62

-- it "run2" $ do
--   run2 "0" `shouldBe` "0"
