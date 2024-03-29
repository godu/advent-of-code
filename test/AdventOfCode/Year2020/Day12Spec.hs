module AdventOfCode.Year2020.Day12Spec
  ( spec,
  )
where

import AdventOfCode.Year2020.Day12
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
      "F10\n\
      \N3\n\
      \F7\n\
      \R90\n\
      \F11\n"
      `shouldBe` "25"
  it "run2" $ do
    run2
      "F10\n\
      \N3\n\
      \F7\n\
      \R90\n\
      \F11\n"
      `shouldBe` "286"
