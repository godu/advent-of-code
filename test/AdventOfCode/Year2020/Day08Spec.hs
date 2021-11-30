module AdventOfCode.Year2020.Day08Spec
  ( spec,
  )
where

import AdventOfCode.Year2020.Day08
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
  it "run2" $ do
    run2
      "nop +0\n\
      \acc +1\n\
      \jmp +4\n\
      \acc +3\n\
      \jmp -3\n\
      \acc -99\n\
      \acc +1\n\
      \jmp -4\n\
      \acc +6\n"
      `shouldBe` "8"
