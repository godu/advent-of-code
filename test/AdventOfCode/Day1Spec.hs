module AdventOfCode.Day1Spec
  ( spec,
  )
where

import AdventOfCode.Day1
  ( process,
    run,
  )
import Test.Hspec
  ( Spec,
    it,
    shouldBe,
  )

spec :: Spec
spec = do
  it "process" $ do
    process [1721, 979, 366, 299, 675, 1456] `shouldBe` 514579
  it "run" $ do
    run "1721\n979\n366\n299\n675\n1456\n" `shouldBe` "514579"
