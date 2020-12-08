module AdventOfCode.Day8Spec
  ( spec,
  )
where

import AdventOfCode.Day8
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
    run1
      "nop +0\n\
      \acc +1\n\
      \jmp +4\n\
      \acc +3\n\
      \jmp -3\n\
      \acc -99\n\
      \acc +1\n\
      \jmp -4\n\
      \acc +6\n"
      `shouldBe` "5"
  it "process2" $ do
    process2 0 `shouldBe` 0
  it "run2" $ do
    run2 "0" `shouldBe` "0"
